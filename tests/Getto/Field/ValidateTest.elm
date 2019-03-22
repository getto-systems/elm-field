module Getto.Field.ValidateTest exposing (..)
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate

import Set

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Validate"
    [ describe "init"
      [ test "should return Field.Model" <|
        \_ ->
          let
            signature = "info"
          in
            "field" |> Validate.init signature ""
            |> Expect.equal ("field" |> Field.init signature () "")
      ]
    , describe "single"
      [ test "should return Validate.Single" <|
        \_ ->
          let
            signature = "info"

            field_ = Form.prop .field (\v m -> { m | field = v })

            model =
              { field = "field" |> Validate.init signature "John"
              }
          in
            model |> ( field_ |> Validate.single [ model.field |> Validate.blank "blank" ] )
            |> Expect.equal ( "field", { field = model.field, prop = field_ }, { errors = [] } )

      , test "should return Validate.Single with empty value" <|
        \_ ->
          let
            signature = "info"

            field_ = Form.prop .field (\v m -> { m | field = v })

            model =
              { field = "field" |> Validate.init signature ""
              }
          in
            model |> ( field_ |> Validate.single [ model.field |> Validate.blank "blank" ] )
            |> Expect.equal ( "field", { field = model.field, prop = field_ }, { errors = ["blank"] } )

      , test "should return Validate.Single with empty set" <|
        \_ ->
          let
            signature = "info"

            field_ = Form.prop .field (\v m -> { m | field = v })

            model =
              { field = "field" |> Validate.init signature Set.empty
              }
          in
            model |> ( field_ |> Validate.single [ model.field |> Validate.emptySet "empty" ] )
            |> Expect.equal ( "field", { field = model.field, prop = field_ }, { errors = ["empty"] } )

      , test "should return Validate.Single with empty list" <|
        \_ ->
          let
            signature = "info"

            field_ = Form.prop .field (\v m -> { m | field = v })

            model =
              { field = "field" |> Validate.init signature []
              }
          in
            model |> ( field_ |> Validate.single [ model.field |> Validate.emptyList "empty" ] )
            |> Expect.equal ( "field", { field = model.field, prop = field_ }, { errors = ["empty"] } )

      , test "should return Validate.Single with not int string" <|
        \_ ->
          let
            signature = "info"

            field_ = Form.prop .field (\v m -> { m | field = v })

            model =
              { field = "field" |> Validate.init signature "string"
              }
          in
            model |> ( field_ |> Validate.single [ model.field |> Validate.notInt "notInt" ] )
            |> Expect.equal ( "field", { field = model.field, prop = field_ }, { errors = ["notInt"] } )
      ]
    , describe "compose"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature ""
              }

            view a =
              { fieldA = a
              }
          in
            Validate.compose view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose2"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature ""
              }

            view a b =
              { fieldA = a
              , fieldB = b
              }
          in
            Validate.compose2 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = ["blank"] } )
                }
              }

      , test "should return View with no error" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              }

            view a b =
              { fieldA = a
              , fieldB = b
              }
          in
            Validate.compose2 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = False
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                }
              }
      ]
    , describe "compose3"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature ""
              }

            view a b c =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              }
          in
            Validate.compose3 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose4"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature ""
              }

            view a b c d =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              }
          in
            Validate.compose4 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose5"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature ""
              }

            view a b c d e =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              }
          in
            Validate.compose5 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose6"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature ""
              }

            view a b c d e f =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              }
          in
            Validate.compose6 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose7"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature ""
              }

            view a b c d e f g =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              }
          in
            Validate.compose7 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose8"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature ""
              }

            view a b c d e f g h =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              }
          in
            Validate.compose8 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose9"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })
            fieldI_ = Form.prop .fieldI (\v m -> { m | fieldI = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature "H"
              , fieldI = "fieldI" |> Validate.init signature ""
              }

            view a b c d e f g h i =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              , fieldI = i
              }
          in
            Validate.compose9 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
              ( model |> ( fieldI_ |> Validate.single [ model.fieldI |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = [] } )
                , fieldI = ( "fieldI", { field = model.fieldI, prop = fieldI_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose10"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })
            fieldI_ = Form.prop .fieldI (\v m -> { m | fieldI = v })
            fieldJ_ = Form.prop .fieldJ (\v m -> { m | fieldJ = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature "H"
              , fieldI = "fieldI" |> Validate.init signature "I"
              , fieldJ = "fieldJ" |> Validate.init signature ""
              }

            view a b c d e f g h i j =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              , fieldI = i
              , fieldJ = j
              }
          in
            Validate.compose10 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
              ( model |> ( fieldI_ |> Validate.single [ model.fieldI |> Validate.blank "blank" ] ) )
              ( model |> ( fieldJ_ |> Validate.single [ model.fieldJ |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = [] } )
                , fieldI = ( "fieldI", { field = model.fieldI, prop = fieldI_ }, { errors = [] } )
                , fieldJ = ( "fieldJ", { field = model.fieldJ, prop = fieldJ_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose11"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })
            fieldI_ = Form.prop .fieldI (\v m -> { m | fieldI = v })
            fieldJ_ = Form.prop .fieldJ (\v m -> { m | fieldJ = v })
            fieldK_ = Form.prop .fieldK (\v m -> { m | fieldK = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature "H"
              , fieldI = "fieldI" |> Validate.init signature "I"
              , fieldJ = "fieldJ" |> Validate.init signature "J"
              , fieldK = "fieldK" |> Validate.init signature ""
              }

            view a b c d e f g h i j k =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              , fieldI = i
              , fieldJ = j
              , fieldK = k
              }
          in
            Validate.compose11 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
              ( model |> ( fieldI_ |> Validate.single [ model.fieldI |> Validate.blank "blank" ] ) )
              ( model |> ( fieldJ_ |> Validate.single [ model.fieldJ |> Validate.blank "blank" ] ) )
              ( model |> ( fieldK_ |> Validate.single [ model.fieldK |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = [] } )
                , fieldI = ( "fieldI", { field = model.fieldI, prop = fieldI_ }, { errors = [] } )
                , fieldJ = ( "fieldJ", { field = model.fieldJ, prop = fieldJ_ }, { errors = [] } )
                , fieldK = ( "fieldK", { field = model.fieldK, prop = fieldK_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose12"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })
            fieldI_ = Form.prop .fieldI (\v m -> { m | fieldI = v })
            fieldJ_ = Form.prop .fieldJ (\v m -> { m | fieldJ = v })
            fieldK_ = Form.prop .fieldK (\v m -> { m | fieldK = v })
            fieldL_ = Form.prop .fieldL (\v m -> { m | fieldL = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature "H"
              , fieldI = "fieldI" |> Validate.init signature "I"
              , fieldJ = "fieldJ" |> Validate.init signature "J"
              , fieldK = "fieldK" |> Validate.init signature "K"
              , fieldL = "fieldL" |> Validate.init signature ""
              }

            view a b c d e f g h i j k l =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              , fieldI = i
              , fieldJ = j
              , fieldK = k
              , fieldL = l
              }
          in
            Validate.compose12 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
              ( model |> ( fieldI_ |> Validate.single [ model.fieldI |> Validate.blank "blank" ] ) )
              ( model |> ( fieldJ_ |> Validate.single [ model.fieldJ |> Validate.blank "blank" ] ) )
              ( model |> ( fieldK_ |> Validate.single [ model.fieldK |> Validate.blank "blank" ] ) )
              ( model |> ( fieldL_ |> Validate.single [ model.fieldL |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = [] } )
                , fieldI = ( "fieldI", { field = model.fieldI, prop = fieldI_ }, { errors = [] } )
                , fieldJ = ( "fieldJ", { field = model.fieldJ, prop = fieldJ_ }, { errors = [] } )
                , fieldK = ( "fieldK", { field = model.fieldK, prop = fieldK_ }, { errors = [] } )
                , fieldL = ( "fieldL", { field = model.fieldL, prop = fieldL_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose13"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })
            fieldI_ = Form.prop .fieldI (\v m -> { m | fieldI = v })
            fieldJ_ = Form.prop .fieldJ (\v m -> { m | fieldJ = v })
            fieldK_ = Form.prop .fieldK (\v m -> { m | fieldK = v })
            fieldL_ = Form.prop .fieldL (\v m -> { m | fieldL = v })
            fieldM_ = Form.prop .fieldM (\v m -> { m | fieldM = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature "H"
              , fieldI = "fieldI" |> Validate.init signature "I"
              , fieldJ = "fieldJ" |> Validate.init signature "J"
              , fieldK = "fieldK" |> Validate.init signature "K"
              , fieldL = "fieldL" |> Validate.init signature "L"
              , fieldM = "fieldM" |> Validate.init signature ""
              }

            view a b c d e f g h i j k l m =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              , fieldI = i
              , fieldJ = j
              , fieldK = k
              , fieldL = l
              , fieldM = m
              }
          in
            Validate.compose13 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
              ( model |> ( fieldI_ |> Validate.single [ model.fieldI |> Validate.blank "blank" ] ) )
              ( model |> ( fieldJ_ |> Validate.single [ model.fieldJ |> Validate.blank "blank" ] ) )
              ( model |> ( fieldK_ |> Validate.single [ model.fieldK |> Validate.blank "blank" ] ) )
              ( model |> ( fieldL_ |> Validate.single [ model.fieldL |> Validate.blank "blank" ] ) )
              ( model |> ( fieldM_ |> Validate.single [ model.fieldM |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = [] } )
                , fieldI = ( "fieldI", { field = model.fieldI, prop = fieldI_ }, { errors = [] } )
                , fieldJ = ( "fieldJ", { field = model.fieldJ, prop = fieldJ_ }, { errors = [] } )
                , fieldK = ( "fieldK", { field = model.fieldK, prop = fieldK_ }, { errors = [] } )
                , fieldL = ( "fieldL", { field = model.fieldL, prop = fieldL_ }, { errors = [] } )
                , fieldM = ( "fieldM", { field = model.fieldM, prop = fieldM_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose14"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })
            fieldI_ = Form.prop .fieldI (\v m -> { m | fieldI = v })
            fieldJ_ = Form.prop .fieldJ (\v m -> { m | fieldJ = v })
            fieldK_ = Form.prop .fieldK (\v m -> { m | fieldK = v })
            fieldL_ = Form.prop .fieldL (\v m -> { m | fieldL = v })
            fieldM_ = Form.prop .fieldM (\v m -> { m | fieldM = v })
            fieldN_ = Form.prop .fieldN (\v m -> { m | fieldN = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature "H"
              , fieldI = "fieldI" |> Validate.init signature "I"
              , fieldJ = "fieldJ" |> Validate.init signature "J"
              , fieldK = "fieldK" |> Validate.init signature "K"
              , fieldL = "fieldL" |> Validate.init signature "L"
              , fieldM = "fieldM" |> Validate.init signature "M"
              , fieldN = "fieldN" |> Validate.init signature ""
              }

            view a b c d e f g h i j k l m n =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              , fieldI = i
              , fieldJ = j
              , fieldK = k
              , fieldL = l
              , fieldM = m
              , fieldN = n
              }
          in
            Validate.compose14 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
              ( model |> ( fieldI_ |> Validate.single [ model.fieldI |> Validate.blank "blank" ] ) )
              ( model |> ( fieldJ_ |> Validate.single [ model.fieldJ |> Validate.blank "blank" ] ) )
              ( model |> ( fieldK_ |> Validate.single [ model.fieldK |> Validate.blank "blank" ] ) )
              ( model |> ( fieldL_ |> Validate.single [ model.fieldL |> Validate.blank "blank" ] ) )
              ( model |> ( fieldM_ |> Validate.single [ model.fieldM |> Validate.blank "blank" ] ) )
              ( model |> ( fieldN_ |> Validate.single [ model.fieldN |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = [] } )
                , fieldI = ( "fieldI", { field = model.fieldI, prop = fieldI_ }, { errors = [] } )
                , fieldJ = ( "fieldJ", { field = model.fieldJ, prop = fieldJ_ }, { errors = [] } )
                , fieldK = ( "fieldK", { field = model.fieldK, prop = fieldK_ }, { errors = [] } )
                , fieldL = ( "fieldL", { field = model.fieldL, prop = fieldL_ }, { errors = [] } )
                , fieldM = ( "fieldM", { field = model.fieldM, prop = fieldM_ }, { errors = [] } )
                , fieldN = ( "fieldN", { field = model.fieldN, prop = fieldN_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose15"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })
            fieldI_ = Form.prop .fieldI (\v m -> { m | fieldI = v })
            fieldJ_ = Form.prop .fieldJ (\v m -> { m | fieldJ = v })
            fieldK_ = Form.prop .fieldK (\v m -> { m | fieldK = v })
            fieldL_ = Form.prop .fieldL (\v m -> { m | fieldL = v })
            fieldM_ = Form.prop .fieldM (\v m -> { m | fieldM = v })
            fieldN_ = Form.prop .fieldN (\v m -> { m | fieldN = v })
            fieldO_ = Form.prop .fieldO (\v m -> { m | fieldO = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature "H"
              , fieldI = "fieldI" |> Validate.init signature "I"
              , fieldJ = "fieldJ" |> Validate.init signature "J"
              , fieldK = "fieldK" |> Validate.init signature "K"
              , fieldL = "fieldL" |> Validate.init signature "L"
              , fieldM = "fieldM" |> Validate.init signature "M"
              , fieldN = "fieldN" |> Validate.init signature "N"
              , fieldO = "fieldO" |> Validate.init signature ""
              }

            view a b c d e f g h i j k l m n o =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              , fieldI = i
              , fieldJ = j
              , fieldK = k
              , fieldL = l
              , fieldM = m
              , fieldN = n
              , fieldO = o
              }
          in
            Validate.compose15 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
              ( model |> ( fieldI_ |> Validate.single [ model.fieldI |> Validate.blank "blank" ] ) )
              ( model |> ( fieldJ_ |> Validate.single [ model.fieldJ |> Validate.blank "blank" ] ) )
              ( model |> ( fieldK_ |> Validate.single [ model.fieldK |> Validate.blank "blank" ] ) )
              ( model |> ( fieldL_ |> Validate.single [ model.fieldL |> Validate.blank "blank" ] ) )
              ( model |> ( fieldM_ |> Validate.single [ model.fieldM |> Validate.blank "blank" ] ) )
              ( model |> ( fieldN_ |> Validate.single [ model.fieldN |> Validate.blank "blank" ] ) )
              ( model |> ( fieldO_ |> Validate.single [ model.fieldO |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = [] } )
                , fieldI = ( "fieldI", { field = model.fieldI, prop = fieldI_ }, { errors = [] } )
                , fieldJ = ( "fieldJ", { field = model.fieldJ, prop = fieldJ_ }, { errors = [] } )
                , fieldK = ( "fieldK", { field = model.fieldK, prop = fieldK_ }, { errors = [] } )
                , fieldL = ( "fieldL", { field = model.fieldL, prop = fieldL_ }, { errors = [] } )
                , fieldM = ( "fieldM", { field = model.fieldM, prop = fieldM_ }, { errors = [] } )
                , fieldN = ( "fieldN", { field = model.fieldN, prop = fieldN_ }, { errors = [] } )
                , fieldO = ( "fieldO", { field = model.fieldO, prop = fieldO_ }, { errors = ["blank"] } )
                }
              }
      ]
    , describe "compose16"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            fieldB_ = Form.prop .fieldB (\v m -> { m | fieldB = v })
            fieldC_ = Form.prop .fieldC (\v m -> { m | fieldC = v })
            fieldD_ = Form.prop .fieldD (\v m -> { m | fieldD = v })
            fieldE_ = Form.prop .fieldE (\v m -> { m | fieldE = v })
            fieldF_ = Form.prop .fieldF (\v m -> { m | fieldF = v })
            fieldG_ = Form.prop .fieldG (\v m -> { m | fieldG = v })
            fieldH_ = Form.prop .fieldH (\v m -> { m | fieldH = v })
            fieldI_ = Form.prop .fieldI (\v m -> { m | fieldI = v })
            fieldJ_ = Form.prop .fieldJ (\v m -> { m | fieldJ = v })
            fieldK_ = Form.prop .fieldK (\v m -> { m | fieldK = v })
            fieldL_ = Form.prop .fieldL (\v m -> { m | fieldL = v })
            fieldM_ = Form.prop .fieldM (\v m -> { m | fieldM = v })
            fieldN_ = Form.prop .fieldN (\v m -> { m | fieldN = v })
            fieldO_ = Form.prop .fieldO (\v m -> { m | fieldO = v })
            fieldP_ = Form.prop .fieldP (\v m -> { m | fieldP = v })

            model =
              { fieldA = "fieldA" |> Validate.init signature "A"
              , fieldB = "fieldB" |> Validate.init signature "B"
              , fieldC = "fieldC" |> Validate.init signature "C"
              , fieldD = "fieldD" |> Validate.init signature "D"
              , fieldE = "fieldE" |> Validate.init signature "E"
              , fieldF = "fieldF" |> Validate.init signature "F"
              , fieldG = "fieldG" |> Validate.init signature "G"
              , fieldH = "fieldH" |> Validate.init signature "H"
              , fieldI = "fieldI" |> Validate.init signature "I"
              , fieldJ = "fieldJ" |> Validate.init signature "J"
              , fieldK = "fieldK" |> Validate.init signature "K"
              , fieldL = "fieldL" |> Validate.init signature "L"
              , fieldM = "fieldM" |> Validate.init signature "M"
              , fieldN = "fieldN" |> Validate.init signature "N"
              , fieldO = "fieldO" |> Validate.init signature "O"
              , fieldP = "fieldP" |> Validate.init signature ""
              }

            view a b c d e f g h i j k l m n o p =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              , fieldG = g
              , fieldH = h
              , fieldI = i
              , fieldJ = j
              , fieldK = k
              , fieldL = l
              , fieldM = m
              , fieldN = n
              , fieldO = o
              , fieldP = p
              }
          in
            Validate.compose16 view
              ( model |> ( fieldA_ |> Validate.single [ model.fieldA |> Validate.blank "blank" ] ) )
              ( model |> ( fieldB_ |> Validate.single [ model.fieldB |> Validate.blank "blank" ] ) )
              ( model |> ( fieldC_ |> Validate.single [ model.fieldC |> Validate.blank "blank" ] ) )
              ( model |> ( fieldD_ |> Validate.single [ model.fieldD |> Validate.blank "blank" ] ) )
              ( model |> ( fieldE_ |> Validate.single [ model.fieldE |> Validate.blank "blank" ] ) )
              ( model |> ( fieldF_ |> Validate.single [ model.fieldF |> Validate.blank "blank" ] ) )
              ( model |> ( fieldG_ |> Validate.single [ model.fieldG |> Validate.blank "blank" ] ) )
              ( model |> ( fieldH_ |> Validate.single [ model.fieldH |> Validate.blank "blank" ] ) )
              ( model |> ( fieldI_ |> Validate.single [ model.fieldI |> Validate.blank "blank" ] ) )
              ( model |> ( fieldJ_ |> Validate.single [ model.fieldJ |> Validate.blank "blank" ] ) )
              ( model |> ( fieldK_ |> Validate.single [ model.fieldK |> Validate.blank "blank" ] ) )
              ( model |> ( fieldL_ |> Validate.single [ model.fieldL |> Validate.blank "blank" ] ) )
              ( model |> ( fieldM_ |> Validate.single [ model.fieldM |> Validate.blank "blank" ] ) )
              ( model |> ( fieldN_ |> Validate.single [ model.fieldN |> Validate.blank "blank" ] ) )
              ( model |> ( fieldO_ |> Validate.single [ model.fieldO |> Validate.blank "blank" ] ) )
              ( model |> ( fieldP_ |> Validate.single [ model.fieldP |> Validate.blank "blank" ] ) )
            |> Expect.equal
              { hasError = True
              , form =
                { fieldA = ( "fieldA", { field = model.fieldA, prop = fieldA_ }, { errors = [] } )
                , fieldB = ( "fieldB", { field = model.fieldB, prop = fieldB_ }, { errors = [] } )
                , fieldC = ( "fieldC", { field = model.fieldC, prop = fieldC_ }, { errors = [] } )
                , fieldD = ( "fieldD", { field = model.fieldD, prop = fieldD_ }, { errors = [] } )
                , fieldE = ( "fieldE", { field = model.fieldE, prop = fieldE_ }, { errors = [] } )
                , fieldF = ( "fieldF", { field = model.fieldF, prop = fieldF_ }, { errors = [] } )
                , fieldG = ( "fieldG", { field = model.fieldG, prop = fieldG_ }, { errors = [] } )
                , fieldH = ( "fieldH", { field = model.fieldH, prop = fieldH_ }, { errors = [] } )
                , fieldI = ( "fieldI", { field = model.fieldI, prop = fieldI_ }, { errors = [] } )
                , fieldJ = ( "fieldJ", { field = model.fieldJ, prop = fieldJ_ }, { errors = [] } )
                , fieldK = ( "fieldK", { field = model.fieldK, prop = fieldK_ }, { errors = [] } )
                , fieldL = ( "fieldL", { field = model.fieldL, prop = fieldL_ }, { errors = [] } )
                , fieldM = ( "fieldM", { field = model.fieldM, prop = fieldM_ }, { errors = [] } )
                , fieldN = ( "fieldN", { field = model.fieldN, prop = fieldN_ }, { errors = [] } )
                , fieldO = ( "fieldO", { field = model.fieldO, prop = fieldO_ }, { errors = [] } )
                , fieldP = ( "fieldP", { field = model.fieldP, prop = fieldP_ }, { errors = ["blank"] } )
                }
              }
      ]
    ]
