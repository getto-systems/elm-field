module Getto.Field.Form exposing
  ( Model
  , Between
  , Prop
  , prop
  , init
  , at
  , setIf
  , set
  , toggle
  , setAttribute
  )

{-| edit field utilities

    type Msg
      = Set (Form.Prop Model attr String) String

    init =
      { name = "field" |> Field.init signature attribute ""
      }

    update msg model =
      case msg of
        Set prop value -> model |> Form.set prop value

    name_ = Form.prop .name (\v m -> { m | name = v })

    view model =
      let
        form =
          { name = model.name |> Form.init name_
          }
      in
        H.input
          [ form.name.field |> Field.value |> A.value
          , form.name.prop |> Set |> E.onInput
          ] []

# Definition
@docs Model, Between, Prop

# Construction
@docs init, prop

# Getter
@docs at

# Update
@docs set, setIf, toggle

# Attribute
@docs setAttribute
 -}


import Getto.Field as Field

import Set exposing ( Set )


{-| field and prop
 -}
type alias Model form attr a =
  { field : Field.Model attr a
  , prop  : Prop form attr a
  }


{-| gteq Model and lteq Model
 -}
type alias Between form attr a =
  { gteq : Model form attr a
  , lteq : Model form attr a
  }


{-| getter and setter
 -}
type Prop form attr a = Prop (Getter form attr a) (Setter form attr a)
type alias Getter form attr a = form -> Field.Model attr a
type alias Setter form attr a = Field.Model attr a -> form -> form


{-| construct Prop

    name_ = Form.prop .name (\v m -> { m | name = v })
 -}
prop : Getter form attr a -> Setter form attr a -> Prop form attr a
prop = Prop


{-| construct Model

    form = field |> Form.init name_
 -}
init : Prop form attr a -> Field.Model attr a -> Model form attr a
init formProp formField =
  { field = formField
  , prop  = formProp
  }


{-| get field from model

    form |> Form.at name_
 -}
at : Prop form attr a -> form -> Field.Model attr a
at (Prop getter _) = getter


{-| set value if value is present

    form |> Form.setIf name_ (Just "value")
    -- set "value"

    form |> Form.setIf name_ Nothing
    -- do nothing
 -}
setIf : Prop form attr a -> Maybe a -> form -> form
setIf formProp value =
  case value of
    Nothing  -> identity
    Just val -> set formProp val


{-| set value

    form |> Form.set name_ "value"
 -}
set : Prop form attr a -> a -> form -> form
set formProp = Field.set >> update formProp


{-| toggle set

    form |> Form.toggle roles_ "admin"
 -}
toggle : Prop form attr (Set comparable) -> comparable -> form -> form
toggle formProp = Field.toggle >> update formProp


{-| set attribute

    form |> Form.setAttribute name_ attr
 -}
setAttribute : Prop form attr value -> attr -> form -> form
setAttribute formProp = Field.setAttribute >> update formProp


update : Prop form attr value -> (Field.Model attr value -> Field.Model attr value) -> form -> form
update (Prop getter setter) f form =
  form |> setter (form |> getter |> f)
