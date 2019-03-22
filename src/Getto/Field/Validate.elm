module Getto.Field.Validate exposing
  ( View
  , Model
  , Single
  , Field
  , Prop
  , Validater
  , init
  , single
  , nothing
  , blank
  , emptySet
  , emptyList
  , notInt
  , validate
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

{-| edit field utilities - validation

    name_ = Form.prop .name (\v m -> { m | name = v })

    model =
      { name = "name" |> Validate.init signature ""
      }

    blank = Validate.blank "blank"

    view a =
      { name = a
      }

    Validate.compose view
      ( model |> ( name_ |> Validate.single [ model.name |> blank ] ) )
    {-
      { hasError = True
      , form =
        { name =
          ( "name"
          , { field = model.name
            , prop  = name_
            }
          , { errors = [ "blank" ] -- model.name |> blank
            }
          )
        }
      }
    -}

# Definition
@docs View, Model, Single, Field, Prop, Validater

# Construction
@docs init, single

# Validaters
@docs nothing, blank, emptySet, emptyList, notInt, validate

# Helper
@docs compose, compose2, compose3, compose4, compose5, compose6, compose7, compose8, compose9, compose10, compose11, compose12, compose13, compose14, compose15, compose16
 -}


import Getto.Field as Field
import Getto.Field.Form as Form

import Set exposing ( Set )


{-| models and that has error
 -}
type alias View model =
  { hasError : Bool
  , form     : model
  }


{-| field name, form and errors
 -}
type alias Model form opts = ( String, form, { opts | errors : List String } )


{-| single form model
 -}
type alias Single form opts a = Model (Form.Model form Attribute a) opts


type alias Attribute = ()


{-| no-attribute field
 -}
type alias Field a = Field.Model Attribute a


{-| Prop for no-attribute field
 -}
type alias Prop form a = Form.Prop form Attribute a


{-| validate method
 -}
type alias Validater attr a = Field.Model attr a -> Maybe String


{-| construct Field

    "name" |> Validate.init signature ""
 -}
init : String -> value -> String -> Field value
init signature = Field.init signature ()


{-| construct Single

    form |> ( name_ |> Validate.single [ form.name |> Validate.blank "blank" ] )
 -}
single : List (Maybe String) -> Form.Prop form attr a -> form -> Model (Form.Model form attr a) {}
single errors prop form =
  let
    field = form |> Form.at prop
  in
    ( field  |> Field.name
    , field  |> Form.init prop
    , { errors = errors |> List.filterMap identity
      }
    )


{-| error if Nothing
 -}
nothing : String -> Validater attr (Maybe value)
nothing error = validate error ((==) Nothing)


{-| error if empty string
 -}
blank : String -> Validater attr String
blank error = validate error String.isEmpty


{-| error if empty set
 -}
emptySet : String -> Validater attr (Set a)
emptySet error = validate error Set.isEmpty


{-| error if empty list
 -}
emptyList : String -> Validater attr (List a)
emptyList error = validate error List.isEmpty


{-| error if string is not int
 -}
notInt : String -> Validater attr String
notInt error = validate error (String.toInt >> (==) Nothing)


{-| error if validation failed
 -}
validate : String -> (a -> Bool) -> Validater attr a
validate error f model =
  if model |> Field.value |> f
    then Just error
    else Nothing


{-| compose models

    type alias Form =
      { name = Validate.Field String
      }

    type alias Model =
      { name = Validate.Single Form {} String
      }

    name_ = Form.prop .name (\v m -> { m | name = v })

    Validate.compose Model
      ( form |> ( name_ |> Validate.single [ form.name |> Validate.blank "blank" ] ) )
 -}
compose : (Model a opts -> model) -> Model a opts -> View model
compose model a =
  { hasError =
    [ a |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a
  }


{-| compose models with 2 args

    Validate.compose Model
      ( form |> ( name_ |> Validate.single [ form.name |> Validate.blank "blank" ] ) )
      ( form |> ( age_  |> Validate.single [] ) )
 -}
compose2 : (Model a opts -> Model b opts -> model) -> Model a opts -> Model b opts -> View model
compose2 model a b =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b
  }


{-| compose models with 3 args
 -}
compose3 : (Model a opts -> Model b opts -> Model c opts -> model) -> Model a opts -> Model b opts -> Model c opts -> View model
compose3 model a b c =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c
  }


{-| compose models with 4 args
 -}
compose4 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> View model
compose4 model a b c d =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d
  }


{-| compose models with 5 args
 -}
compose5 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> View model
compose5 model a b c d e =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e
  }


{-| compose models with 6 args
 -}
compose6 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> View model
compose6 model a b c d e f =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f
  }


{-| compose models with 7 args
 -}
compose7 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> View model
compose7 model a b c d e f g =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g
  }


{-| compose models with 8 args
 -}
compose8 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> View model
compose8 model a b c d e f g h =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h
  }


{-| compose models with 9 args
 -}
compose9 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> View model
compose9 model a b c d e f g h i =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i
  }


{-| compose models with 10 args
 -}
compose10 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> View model
compose10 model a b c d e f g h i j =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j
  }


{-| compose models with 11 args
 -}
compose11 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> View model
compose11 model a b c d e f g h i j k =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k
  }


{-| compose models with 12 args
 -}
compose12 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> View model
compose12 model a b c d e f g h i j k l =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l
  }


{-| compose models with 13 args
 -}
compose13 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> View model
compose13 model a b c d e f g h i j k l m =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    , m |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l m
  }


{-| compose models with 14 args
 -}
compose14 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> View model
compose14 model a b c d e f g h i j k l m n =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    , m |> toErrors
    , n |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l m n
  }


{-| compose models with 15 args
 -}
compose15 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> View model
compose15 model a b c d e f g h i j k l m n o =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    , m |> toErrors
    , n |> toErrors
    , o |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l m n o
  }


{-| compose models with 16 args
 -}
compose16 : (Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> Model p opts -> model) -> Model a opts -> Model b opts -> Model c opts -> Model d opts -> Model e opts -> Model f opts -> Model g opts -> Model h opts -> Model i opts -> Model j opts -> Model k opts -> Model l opts -> Model m opts -> Model n opts -> Model o opts -> Model p opts -> View model
compose16 model a b c d e f g h i j k l m n o p =
  { hasError =
    [ a |> toErrors
    , b |> toErrors
    , c |> toErrors
    , d |> toErrors
    , e |> toErrors
    , f |> toErrors
    , g |> toErrors
    , h |> toErrors
    , i |> toErrors
    , j |> toErrors
    , k |> toErrors
    , l |> toErrors
    , m |> toErrors
    , n |> toErrors
    , o |> toErrors
    , p |> toErrors
    ] |> List.concat |> List.isEmpty |> not
  , form = model a b c d e f g h i j k l m n o p
  }

toErrors : Model form m -> List String
toErrors (_,_,m) = m.errors
