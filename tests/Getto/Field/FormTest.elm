module Getto.Field.FormTest exposing (..)
import Getto.Field as Field
import Getto.Field.Form as Form

import Set

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Form"
    [ describe "init"
      [ test "should return Form.Model" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "option" }

            model =
              { name = "field" |> Field.init signature attribute ""
              }

            name_ = Form.prop .name (\v m -> { m | name = v })
          in
            model |> Form.at name_
            |> Expect.equal model.name
      ]
    , describe "setIf"
      [ test "should set value if value present" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "option" }

            model =
              { name = "field" |> Field.init signature attribute ""
              }

            name_ = Form.prop .name (\v m -> { m | name = v })
          in
            model |> Form.setIf name_ (Just "value") |> Form.at name_ |> Field.value
            |> Expect.equal "value"

      , test "should not set value if value not present" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "option" }

            model =
              { name = "field" |> Field.init signature attribute "default"
              }

            name_ = Form.prop .name (\v m -> { m | name = v })
          in
            model |> Form.setIf name_ Nothing |> Form.at name_ |> Field.value
            |> Expect.equal "default"
      ]
    , describe "set"
      [ test "should set value" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "option" }

            model =
              { name = "field" |> Field.init signature attribute ""
              }

            name_ = Form.prop .name (\v m -> { m | name = v })
          in
            model |> Form.set name_ "value" |> Form.at name_ |> Field.value
            |> Expect.equal "value"
      ]
    , describe "toggle"
      [ test "should toggle set" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "option" }

            model =
              { roles = "field" |> Field.init signature attribute Set.empty
              }

            roles_ = Form.prop .roles (\v m -> { m | roles = v })
          in
            model |> Form.toggle roles_ "value" |> Form.at roles_ |> Field.value
            |> Expect.equal (Set.singleton "value")
      ]
    , describe "setAttribute"
      [ test "should set field attribute" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "" }

            model =
              { name = "field" |> Field.init signature attribute ""
              }

            name_ = Form.prop .name (\v m -> { m | name = v })
          in
            model |> Form.setAttribute name_ { key = "option" } |> Form.at name_ |> Field.attribute
            |> Expect.equal { key = "option" }
      ]
    ]
