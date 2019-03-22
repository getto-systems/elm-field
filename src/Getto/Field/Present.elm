module Getto.Field.Present exposing
  ( Model
  , Single
  , Between
  , Field
  , Prop
  , Presenter
  , init
  , single
  , between
  , string
  , set
  , present
  )

{-| search field utilities - present

    name_ = Form.prop .name (\v m -> { m | name = v })

    model =
      { name = "name" |> Present.init signature "John"
      }

    view =
      { name = model |> ( name_ |> Present.single Present.string )
      {-
        ( "name"
        , { field = model.name
          , prop  = name_
          }
        , { isPresent = True -- model.name |> Present.string
          }
        )
      -}
      }

# Definition
@docs Model, Single, Between, Field, Prop, Presenter

# Construction
@docs init, single, between

# Presenters
@docs string, set, present
 -}


import Getto.Field as Field
import Getto.Field.Form as Form

import Set exposing ( Set )


{-| field name, form and isPresent
 -}
type alias Model form opts = ( String, form, { opts | isPresent : Bool } )


{-| single value model
 -}
type alias Single  form opts a = Model (Form.Model   form Attribute a) opts


{-| gteq-lteq value model
 -}
type alias Between form opts a = Model (Form.Between form Attribute a) opts


type alias Attribute = ()


{-| no-attribute Field
 -}
type alias Field a = Field.Model Attribute a


{-| Prop for no-attribute Field
 -}
type alias Prop form a = Form.Prop form Attribute a


{-| presence method
 -}
type alias Presenter attr a = Field.Model attr a -> Bool


{-| construct Field

    "name" |> Present.init signature ""
 -}
init : String -> value -> String -> Field value
init signature = Field.init signature ()


{-| construct Single

    form |> ( name_ |> Present.single Present.string )
 -}
single : Presenter attr a -> Form.Prop form attr a -> form -> Model (Form.Model form attr a) {}
single f prop model =
  let
    field = model |> Form.at prop
  in
    ( field |> Field.name
    , field |> Form.init prop
    , { isPresent = field |> f
      }
    )


{-| construct Between

    form |>
      ( "age" |> Present.between
        { gteq = age_gteq_ |> Present.single Present.string
        , lteq = age_lteq_ |> Present.single Present.string
        }
      )
 -}
between : { gteq : model -> Model (Form.Model form attr a) opts, lteq : model -> Model (Form.Model form attr a) opts } -> String -> model -> Model (Form.Between form attr a) {}
between units name model =
  let
    gteq = model |> units.gteq
    lteq = model |> units.lteq

    isPresent (_,_,opts) = opts.isPresent
    getForm   (_,form,_) = form
  in
    ( name
    , { gteq = gteq |> getForm
      , lteq = lteq |> getForm
      }
    , { isPresent = ( gteq |> isPresent ) || ( lteq |> isPresent )
      }
    )


{-| string is not empty
 -}
string : Presenter attr String
string = present (String.isEmpty >> not)


{-| set is not empty
 -}
set : Presenter attr (Set a)
set = present (Set.isEmpty >> not)


{-| field value is present
 -}
present : (a -> Bool) -> Presenter attr a
present f = Field.value >> f
