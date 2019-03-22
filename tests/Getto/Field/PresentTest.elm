module Getto.Field.PresentTest exposing (..)
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Present as Present

import Set

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Present"
    [ describe "init"
      [ test "should return Field.Model" <|
        \_ ->
          let
            signature = "info"
          in
            "name" |> Present.init signature ""
            |> Expect.equal ("name" |> Field.init signature () "")
      ]
    , describe "single"
      [ test "should return Present.Single" <|
        \_ ->
          let
            signature = "info"

            name_ = Form.prop .name (\v m -> { m | name = v })

            model =
              { name = "name" |> Present.init signature "John"
              }
          in
            model |> ( name_ |> Present.single Present.string )
            |> Expect.equal ( "name", { field = model.name, prop = name_ }, { isPresent = True } )

      , test "should return Present.Single with empty value" <|
        \_ ->
          let
            signature = "info"

            name_ = Form.prop .name (\v m -> { m | name = v })

            model =
              { name = "name" |> Present.init signature ""
              }
          in
            model |> ( name_ |> Present.single Present.string )
            |> Expect.equal ( "name", { field = model.name, prop = name_ }, { isPresent = False } )

      , test "should return Present.Single with set" <|
        \_ ->
          let
            signature = "info"

            name_ = Form.prop .name (\v m -> { m | name = v })

            model =
              { name = "name" |> Present.init signature (Set.singleton "value")
              }
          in
            model |> ( name_ |> Present.single Present.set )
            |> Expect.equal ( "name", { field = model.name, prop = name_ }, { isPresent = True } )

      , test "should return Present.Single with empty set" <|
        \_ ->
          let
            signature = "info"

            name_ = Form.prop .name (\v m -> { m | name = v })

            model =
              { name = "name" |> Present.init signature Set.empty
              }
          in
            model |> ( name_ |> Present.single Present.set )
            |> Expect.equal ( "name", { field = model.name, prop = name_ }, { isPresent = False } )
      ]
    , describe "between"
      [ test "should return Present.Between" <|
        \_ ->
          let
            signature = "info"

            age_gteq_ = Form.prop .age_gteq (\v m -> { m | age_gteq = v })
            age_lteq_ = Form.prop .age_lteq (\v m -> { m | age_lteq = v })

            model =
              { age_gteq = "age_gteq" |> Present.init signature "30"
              , age_lteq = "age_lteq" |> Present.init signature "40"
              }
          in
            model |>
              ( "age" |> Present.between
                { gteq = age_gteq_ |> Present.single Present.string
                , lteq = age_lteq_ |> Present.single Present.string
                }
              )
            |> Expect.equal
              ( "age"
              , { gteq = { field = model.age_gteq, prop = age_gteq_ }
                , lteq = { field = model.age_lteq, prop = age_lteq_ }
                }
              , { isPresent = True }
              )

      , test "should return Present.Between with one empty value" <|
        \_ ->
          let
            signature = "info"

            age_gteq_ = Form.prop .age_gteq (\v m -> { m | age_gteq = v })
            age_lteq_ = Form.prop .age_lteq (\v m -> { m | age_lteq = v })

            model =
              { age_gteq = "age_gteq" |> Present.init signature "30"
              , age_lteq = "age_lteq" |> Present.init signature ""
              }
          in
            model |>
              ( "age" |> Present.between
                { gteq = age_gteq_ |> Present.single Present.string
                , lteq = age_lteq_ |> Present.single Present.string
                }
              )
            |> Expect.equal
              ( "age"
              , { gteq = { field = model.age_gteq, prop = age_gteq_ }
                , lteq = { field = model.age_lteq, prop = age_lteq_ }
                }
              , { isPresent = True }
              )

      , test "should return Present.Between with all empty value" <|
        \_ ->
          let
            signature = "info"

            age_gteq_ = Form.prop .age_gteq (\v m -> { m | age_gteq = v })
            age_lteq_ = Form.prop .age_lteq (\v m -> { m | age_lteq = v })

            model =
              { age_gteq = "age_gteq" |> Present.init signature ""
              , age_lteq = "age_lteq" |> Present.init signature ""
              }
          in
            model |>
              ( "age" |> Present.between
                { gteq = age_gteq_ |> Present.single Present.string
                , lteq = age_lteq_ |> Present.single Present.string
                }
              )
            |> Expect.equal
              ( "age"
              , { gteq = { field = model.age_gteq, prop = age_gteq_ }
                , lteq = { field = model.age_lteq, prop = age_lteq_ }
                }
              , { isPresent = False }
              )
      ]
    ]
