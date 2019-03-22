module Getto.Field.Edit exposing
  ( Form
  , View
  , AggregateState(..)
  , form
  , options
  , view
  , fields
  , update
  , edit
  , cancel
  , commit
  , change
  , encode
  , decode
  )

{-| edit field utilities

    fields =
      { name = "name" |> Conflict.init signature ""
      }

    form = fields |> Edit.form

    name_ = Form.prop .name (\v m -> { m | name = v })
    get_name = .name

    blank = Validate.blank "blank"

    (isStatic,options) =
      { error       = "conflict"
      , form        = form
      , last        = http.get.response
      , isConflict  = http.put.isConflict
      , isDifferent = response.isDifferentResponse
      }
      |> Edit.options

    view a =
      { name = a
      }

    ( isStatic
    , Conflict.compose view
      ( options |> ( ( name_,  get_name  ) |> Conflict.single [ fields.name |> V.blank ] ) )
    )
    |> Edit.view
    {-
      { isStatic = True
      , state = HasError
      , form =
        { name =
          ( "name"
          , { field = model.name
            , prop  = name_
            }
          , { errors = [ "blank" ] -- model.name |> blank
            , isModified = True
            }
          )
        }
      }
    -}

# Definition
@docs Form, View, AggregateState

# Construction
@docs form, options, view

# Getter
@docs fields

# Update
@docs update, edit, cancel, commit, change

# Encode/Decode
@docs encode, decode
 -}


import Getto.Field as Field
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode


{-| edit/static state and fields
 -}
type Form response fields = Form (State response) fields


{-| isStatic, state (HasError, HasModified) and form
 -}
type alias View model =
  { isStatic : Bool
  , state    : AggregateState
  , form     : model
  }


{-| initialize options

    { error : String               -- error message
    , form  : Form response fields -- form
    , last  : response             -- last response
    , isConflict  : Bool           -- if put response is conflict
    , isDifferent :
      response -> response -> Bool -- if responses are different
    }
 -}
type alias Options response fields =
  { error       : String
  , form        : Form response fields
  , last        : response
  , isConflict  : Bool
  , isDifferent : response -> response -> Bool
  }


{-| static/edit state
 -}
type State response
  = Static
  | Edit Bool response


{-| fields HasError or HasModified or Same
 -}
type AggregateState
  = Same
  | HasError
  | HasModified


{-| construct form

    { name = "name" |> Conflict.init signature ""
    }
    |> Edit.form
 -}
form : fields -> Form response fields
form = Form Static


{-| construct Conflict.Options

    { error       = "conflict"
    , form        = form
    , last        = http.get.response
    , isConflict  = http.put.isConflict
    , isDifferent = response.isDifferentResponse
    }
    |> Edit.options
    -- ( isStatic, options )
 -}
options : Options response fields -> ( Bool, Conflict.Options (Form response fields) response )
options opts =
  let
    (Form state _) = opts.form

    response =
      case state of
        Static -> Nothing
        Edit _ res -> res |> Just

    isDifferent = response |> Maybe.map (opts.last |> opts.isDifferent) |> Maybe.withDefault False

    isStatic =
      case state of
        Static -> True
        Edit isCommit _ ->
          opts.isConflict ||
          ( isCommit && isDifferent )
  in
    ( isStatic
    , { error = opts.error
      , form  = opts.form
      , response =
        { first = response
        , last  = opts.last
        }
      }
    )


{-| construct Conflict.Options

    let
      fields = form |> Edit.fields
    in
      ( isStatic
      , Conflict.compose view
        ( options |> ( ( name_,  get_name  ) |> Conflict.single [ fields.name |> V.blank ] ) )
      )
      |> Edit.view
 -}
view : ( Bool, Conflict.View model ) -> View model
view (isStatic,conflictView) =
  { isStatic = isStatic
  , form = conflictView.form
  , state =
    if conflictView.hasError
      then HasError
      else
        if conflictView.hasModified
          then HasModified
          else Same
  }


{-| form fields
 -}
fields : Form response fields -> fields
fields (Form _ model) = model


{-| update fields
 -}
update : (fields -> fields) -> Form response fields -> Form response fields
update f (Form state model) = Form state (model |> f)


{-| to edit state
 -}
edit : (response -> Form response fields -> Form response fields) -> Maybe response -> Form response fields -> Form response fields
edit editFields value (Form state model) =
  case value of
    Nothing  -> Form state model
    Just res -> Form (Edit False res) model |> editFields res


{-| to static state
 -}
cancel : Form response fields -> Form response fields
cancel (Form _ model) = Form Static model


{-| to commit state
 -}
commit : Form response fields -> Form response fields
commit (Form state model) =
  case state of
    Edit _ res -> Form (Edit True res) model
    _          -> Form state model


{-| to changed state
 -}
change : Form response fields -> Form response fields
change (Form state model) =
  case state of
    Edit _ res -> Form (Edit False res) model
    _          -> Form state model


{-| encode fields
 -}
encode : (response -> Encode.Value) -> (fields -> Encode.Value) -> Form response fields -> Encode.Value
encode encodeResponse encodeModel (Form state model) =
  case state of
    Static -> [ ( "state", "static" |> Encode.string ) ] |> Encode.object
    Edit _ res ->
      [ ( "state",   "edit" |> Encode.string )
      , ( "response", res   |> encodeResponse )
      , ( "model",    model |> encodeModel )
      ] |> Encode.object


{-| decode fields
 -}
decode : Decode.Decoder response -> (Decode.Value -> Form response fields -> Form response fields) -> Decode.Value -> Form response fields -> Form response fields
decode decodeResponse decodeModel value (Form state model) =
  case
    ( value |> decodeValue "state"    Decode.string
    , value |> decodeValue "response" decodeResponse
    , value |> decodeValue "model"    Decode.value
    )
  of
    ( Just "edit", Just res, Just val ) -> Form (Edit False res) model |> decodeModel val
    _                                   -> Form state model

decodeValue : String -> Decode.Decoder a -> Decode.Value -> Maybe a
decodeValue key decoder = Decode.decodeValue (Decode.at [key] decoder) >> Result.toMaybe
