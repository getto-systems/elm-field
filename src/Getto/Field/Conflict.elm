module Getto.Field.Conflict exposing
  ( View
  , Model
  , Single
  , Attribute
  , Field
  , Form
  , Prop
  , Getter
  , Options
  , State(..)
  , Resolve
  , init
  , single
  , state
  , resolve
  , leave
  , revert
  , toConflict
  , compose
  , compose2
  , compose3
  , compose4
  , compose5
  , compose6
  , compose7
  , compose8
  , compose9
  , compose10
  , compose11
  , compose12
  , compose13
  , compose14
  , compose15
  , compose16
  )

{-| edit field utilities - conflict

    name_ = Form.prop .name (\v m -> { m | name = v })
    get_name = .name

    model =
      { name = "name" |> Conflict.init signature ""
      }

    blank = Validate.blank "blank"

    opts =
      { error = "conflict"
      , form  = model
      , response =
        { first = form.response
        , last  = http.response
        }
      }

    view a =
      { name = a
      }

    Conflict.compose view
      ( model |> ( ( name_, get_name ) |> Conflict.single [ model.name |> blank ] ) )
    {-
      { hasError = True
      , hasModified = True
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
@docs View, Model, Single, Attribute, Field, Form, Prop, Getter, Options, State, Resolve

# Construction
@docs init, single

# Getter
@docs state

# Setter
@docs toConflict

# Resolve
@docs resolve, leave, revert

# Helper
@docs compose, compose2, compose3, compose4, compose5, compose6, compose7, compose8, compose9, compose10, compose11, compose12, compose13, compose14, compose15, compose16
 -}


import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate


{-| models and that has error, has modified
 -}
type alias View model =
  { hasError    : Bool
  , hasModified : Bool
  , form        : model
  }


{-| field name, form and errors
 -}
type alias Model form opts = Validate.Model form { opts | isModified : Bool }


{-| single form model
 -}
type alias Single form opts a = Model (Form form a) opts


{-| attribute of conflict state and resolve operation
 -}
type Attribute a = Attribute
  { state     : State a
  , operation : Operation
  }


{-| conflict attribute field
 -}
type alias Field a = Field.Model (Attribute a) a


{-| Prop for conflict attribute field
 -}
type alias Prop form a = Form.Prop form (Attribute a) a


{-| response to value getter
 -}
type alias Getter response a = response -> a


{-| field, prop and last value
 -}
type alias Form form a =
  { field : Field a
  , prop  : Prop form a
  , last  : a
  }


{-| initialize options

    { error : String -- error message
    , form  : form   -- fields
    , response :
      { first : Maybe response -- response of edit start
      , last  : response       -- last response
      }
    }
 -}
type alias Options form response =
  { error : String
  , form  : form
  , response :
    { first : Maybe response
    , last  : response
    }
  }


{-| conflict state
 -}
type State lastValue
  = NoProblem
  | Conflict lastValue


{-| resolve operation
 -}
type Operation
  = None
  | OverWrite


{-| resolve type
 -}
type Resolve value
  = Leave
  | Revert value


{-| construct Field

    "name" |> Conflict.init signature ""
 -}
init : String -> value -> String -> Field value
init signature = Field.init signature
  ( Attribute
    { state     = NoProblem
    , operation = None
    }
  )


{-| construct Single

    { error = "conflict"
    , form  = form
    , response =
      { first = form.response
      , last  = http.response
      }
    }
    |> ( ( name_, get_name ) |> Conflict.single [ form.name |> Validate.blank "blank" ] )
 -}
single : List (Maybe String) -> ( Prop form a, Getter response a ) -> Options form response -> Model (Form form a) {}
single errors (prop,getter) model =
  let
    (name,form,opts) = model.form |> ( prop |> Validate.single errors )

    field = form.field
    (Attribute attribute) = form.field |> Field.attribute

    lastValue = model.response.last |> getter
    formValue = field |> Field.value

    isModified = lastValue /= formValue

    newState =
      case model.response.first |> Maybe.map getter of
        Just firstValue ->
          case attribute.operation of
            OverWrite -> NoProblem
            None ->
              if ( firstValue /= lastValue ) &&
                 ( firstValue /= formValue ) &&
                 ( formValue  /= lastValue )
                then Conflict lastValue
                else NoProblem

        _ -> NoProblem

    conflictError =
      case newState of
        NoProblem  -> []
        Conflict _ -> [model.error]
  in
    ( name
    , { field = field |> Field.setAttribute
        ( Attribute { attribute | state = newState } )
      , prop = form.prop
      , last = lastValue
      }
    , { errors = opts.errors ++ conflictError
      , isModified = isModified
      }
    )


{-| get conflict state

    form |> Conflict.state
 -}
state : Form form a -> State a
state = .field >> Field.attribute >> (\(Attribute attribute) -> attribute.state )


{-| set conflict
 -}
toConflict : a -> Field a -> Field a
toConflict value field =
  let
    (Attribute attribute) = field |> Field.attribute
  in
    field |> Field.setAttribute
      ( Attribute { attribute | state = Conflict value } )


{-| resolve conflict

    model |> Conflict.resolve prop Conflict.leave
    model |> Conflict.resolve prop (Conflict.revert value)
 -}
resolve : Prop form a -> Resolve a -> form -> form
resolve prop mig form =
  let
    field = form |> Form.at prop
    (Attribute attribute) = field |> Field.attribute
  in
    form
    |> Form.setAttribute prop
      ( Attribute
        { attribute
        | state     = NoProblem
        , operation = OverWrite
        }
      )
    |>
      ( case mig of
        Leave        -> identity
        Revert value -> Form.set prop value
      )


{-| leave editing value
 -}
leave : Resolve a
leave = Leave


{-| revert original value
 -}
revert : a -> Resolve a
revert = Revert


{-| compose models

    type alias Form =
      { name = Conflict.Field String
      }

    type alias Model =
      { name = Conflict.Single Form {} String
      }

    name_ = Form.prop .name (\v m -> { m | name = v })
    get_name = .name

    opts =
      { error = "conflict"
      , form  = form
      , response =
        { first = form.response
        , last  = http.response
        }
      }

    Conflict.compose Model
      ( opts |> ( ( name_, get_name ) |> Conflict.single [ form.name |> Validate.blank "blank" ] ) )
 -}
compose : (Model a opts -> model) -> Model a opts -> View model
compose model a =
  let
    view = Validate.compose model a
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 2 args

    Conflict.compose2 Model
      ( opts |> ( ( name_, get_name ) |> Conflict.single [ form.name |> Validate.blank "blank" ] ) )
      ( opts |> ( ( age_,  get_age )  |> Conflict.single [] ) )
 -}
compose2 : (Model a opts -> Model b opts -> model) -> Model a opts -> Model b opts -> View model
compose2 model a b =
  let
    view = Validate.compose2 model a b
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 3 args
 -}
compose3 : (Model a opts -> Model b opts -> Model c opts -> model) -> Model a opts -> Model b opts -> Model c opts -> View model
compose3 model a b c =
  let
    view = Validate.compose3 model a b c
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 4 args
 -}
compose4 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> View model
compose4 model a b c d =
  let
    view = Validate.compose4 model a b c d
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 5 args
 -}
compose5 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> View model
compose5 model a b c d e =
  let
    view = Validate.compose5 model a b c d e
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 6 args
 -}
compose6 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> View model
compose6 model a b c d e f =
  let
    view = Validate.compose6 model a b c d e f
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 7 args
 -}
compose7 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> View model
compose7 model a b c d e f g =
  let
    view = Validate.compose7 model a b c d e f g
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 8 args
 -}
compose8 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> View model
compose8 model a b c d e f g h =
  let
    view = Validate.compose8 model a b c d e f g h
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 9 args
 -}
compose9 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> View model
compose9 model a b c d e f g h i =
  let
    view = Validate.compose9 model a b c d e f g h i
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      , i |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 10 args
 -}
compose10 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> View model
compose10 model a b c d e f g h i j =
  let
    view = Validate.compose10 model a b c d e f g h i j
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      , i |> modified
      , j |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 11 args
 -}
compose11 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> View model
compose11 model a b c d e f g h i j k =
  let
    view = Validate.compose11 model a b c d e f g h i j k
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      , i |> modified
      , j |> modified
      , k |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 12 args
 -}
compose12 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> View model
compose12 model a b c d e f g h i j k l =
  let
    view = Validate.compose12 model a b c d e f g h i j k l
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      , i |> modified
      , j |> modified
      , k |> modified
      , l |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 13 args
 -}
compose13 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> View model
compose13 model a b c d e f g h i j k l m =
  let
    view = Validate.compose13 model a b c d e f g h i j k l m
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      , i |> modified
      , j |> modified
      , k |> modified
      , l |> modified
      , m |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 14 args
 -}
compose14 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> View model
compose14 model a b c d e f g h i j k l m n =
  let
    view = Validate.compose14 model a b c d e f g h i j k l m n
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      , i |> modified
      , j |> modified
      , k |> modified
      , l |> modified
      , m |> modified
      , n |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 15 args
 -}
compose15 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> View model
compose15 model a b c d e f g h i j k l m n o =
  let
    view = Validate.compose15 model a b c d e f g h i j k l m n o
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      , i |> modified
      , j |> modified
      , k |> modified
      , l |> modified
      , m |> modified
      , n |> modified
      , o |> modified
      ] |> List.any identity
    , form = view.form
    }


{-| compose models with 16 args
 -}
compose16 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> Model p opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> Model p opts -> View model
compose16 model a b c d e f g h i j k l m n o p =
  let
    view = Validate.compose16 model a b c d e f g h i j k l m n o p
  in
    { hasError = view.hasError
    , hasModified =
      [ a |> modified
      , b |> modified
      , c |> modified
      , d |> modified
      , e |> modified
      , f |> modified
      , g |> modified
      , h |> modified
      , i |> modified
      , j |> modified
      , k |> modified
      , l |> modified
      , m |> modified
      , n |> modified
      , o |> modified
      , p |> modified
      ] |> List.any identity
    , form = view.form
    }

modified : Model form m -> Bool
modified (_,_,opts) = opts.isModified
