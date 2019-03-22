module Getto.FieldTest exposing (..)
import Getto.Field as Field

import Set

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Field"
    [ describe "init"
      [ test "should return Field.Model" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "option" }
            field = "field" |> Field.init signature attribute "value"
          in
            ( ( field |> Field.id
              , field |> Field.name
              , field |> Field.value
              )
            , ( field |> Field.id_value, field |> Field.name_value )
            , field |> Field.attribute
            )
            |> Expect.equal
              ( ( "info-field", "field", "value" )
              , ( ( "info-field", "value" )
                , ( "field",      "value" )
                )
              , { key = "option" }
              )
      ]
    , describe "param"
      [ test "should return Param if modified" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "" }
            field = "field" |> Field.init signature attribute "value"
          in
            field |> Field.param ""
            |> Expect.equal ( Just ( "field", { before = "", after = "value" } ) )

      , test "should return Nothing if not modified" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "" }
            field = "field" |> Field.init signature attribute "value"
          in
            field |> Field.param "value"
            |> Expect.equal Nothing
      ]
    , describe "set"
      [ test "should set field value" <|
        \_ ->
          let
            signature = "info"
            attribute = ()
            field = "field" |> Field.init signature attribute ""
          in
            field |> Field.set "value" |> Field.value
            |> Expect.equal "value"
      ]
    , describe "toggle"
      [ test "should toggle field set" <|
        \_ ->
          let
            signature = "info"
            attribute = ()
            field = "field" |> Field.init signature attribute Set.empty
          in
            field |> Field.toggle "value" |> Field.value
            |> Expect.equal (Set.singleton "value")
      ]
    , describe "setAttribute"
      [ test "should set field attribute" <|
        \_ ->
          let
            signature = "info"
            attribute = { key = "" }
            field = "field" |> Field.init signature attribute ""
          in
            field |> Field.setAttribute { key = "option" } |> Field.attribute
            |> Expect.equal { key = "option" }
      ]
    ]
