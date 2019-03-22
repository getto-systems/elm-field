module Getto.Field exposing
  ( Model
  , Param
  , init
  , id
  , name
  , value
  , id_value
  , name_value
  , param
  , set
  , toggle
  , attribute
  , setAttribute
  )

{-| edit field utilities

    type Msg
      = Set String

    init = "name" |> Field.init signature attr ""

    update msg model =
      Set value -> ( field |> Field.set value, Cmd.none )

    view model =
      H.input
        [ model |> Field.value |> A.value
        , Set |> E.onInput
        ] []

# Definition
@docs Model, Param

# Construction
@docs init

# Getter
@docs id, name, value, id_value, name_value, param

# Update
@docs set, toggle

# Attribute
@docs attribute, setAttribute
 -}


import Set exposing ( Set )


{-| field attribute and id, name value
 -}
type Model attr value = Model attr
  { id    : String
  , name  : String
  , value : value
  }

{-| field original value (before) and modified value (after)
 -}
type alias Param a =
  { before : a
  , after  : a
  }


{-| construct model from signature, attribute, default value, field name

    signature = "info"
    attr = ()

    field = "name" |> Field.init signature attr ""
 -}
init : String -> attr -> value -> String -> Model attr value
init signature attr defaultValue fieldName = Model attr
  { id    = signature ++ "-" ++ fieldName
  , name  = fieldName
  , value = defaultValue
  }


{-| field id

    H.input [ field |> Field.id |> A.id ] []
 -}
id : Model attr value -> String
id (Model _ model) = model.id


{-| field name

    H.input [ field |> Field.name |> A.name ] []
 -}
name : Model attr value -> String
name (Model _ model) = model.name


{-| field value

    H.input [ field |> Field.value |> A.value ] []
 -}
value : Model attr value -> value
value (Model _ model) = model.value


{-| ( id, value )
 -}
id_value : Model attr value -> ( String, value )
id_value model = ( model |> id, model |> value )


{-| ( name, value )
 -}
name_value : Model attr value -> ( String, value )
name_value model = ( model |> name, model |> value )


{-| maybe ( name, Param )
 -}
param : value -> Model attr value -> Maybe ( String, Param value )
param lastValue = name_value >>
  \(fieldName,formValue) ->
    if lastValue == formValue
      then Nothing
      else Just
        ( fieldName
        , { before = lastValue
          , after  = formValue
          }
        )


{-| set value

    field = field |> Field.set "John"
 -}
set : value -> Model attr value -> Model attr value
set val (Model attr model) = Model attr { model | value = val }


{-| toggle Set entry

    field = field |> Field.toggle "admin" }
 -}
toggle : comparable -> Model attr (Set comparable) -> Model attr (Set comparable)
toggle val (Model attr model) =
  Model attr
    { model
    | value =
      if model.value |> Set.member val
        then model.value |> Set.remove val
        else model.value |> Set.insert val
    }


{-| field attribute
 -}
attribute : Model attr value -> attr
attribute (Model attr _) = attr


{-| set attribute
 -}
setAttribute : attr -> Model attr value -> Model attr value
setAttribute attr (Model _ model) = Model attr model
