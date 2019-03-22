module Getto.Field.ConflictTest exposing (..)
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Conflict as Conflict

import Set

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Conflict"
    [ describe "init"
      [ test "should return Field.Model" <|
        \_ ->
          let
            signature = "info"
          in
            "field" |> Conflict.init signature ""
            |> Field.id_value
            |> Expect.equal ( "info-field", "" )
      ]
    , describe "single"
      [ test "should return Conflict.Single" <|
        \_ ->
          let
            signature = "info"

            field_ = Form.prop .field (\v m -> { m | field = v })
            get_field = .field

            model =
              { field = "field" |> Conflict.init signature "John"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = { field = "field" } |> Just
                , last  = { field = "field" }
                }
              }
          in
            opts |> ( ( field_, get_field ) |> Conflict.single [] )
            |> Expect.equal
              ( "field"
              , { field = model.field, prop = field_, last = "field" }
              , { errors = [], isModified = True }
              )

      , test "should return Conflict.Single with not modified" <|
        \_ ->
          let
            signature = "info"

            field_ = Form.prop .field (\v m -> { m | field = v })
            get_field = .field

            model =
              { field = "field" |> Conflict.init signature "field"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = { field = "field" } |> Just
                , last  = { field = "field" }
                }
              }
          in
            opts |> ( ( field_, get_field ) |> Conflict.single [] )
            |> Expect.equal
              ( "field"
              , { field = model.field, prop = field_, last = "field" }
              , { errors = [], isModified = False }
              )

      , test "should return Conflict.Single with conflict" <|
        \_ ->
          let
            signature = "info"

            field_ = Form.prop .field (\v m -> { m | field = v })
            get_field = .field

            model =
              { field = "field" |> Conflict.init signature "modified"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = { field = "field" } |> Just
                , last  = { field = "conflict" }
                }
              }
          in
            opts |> ( ( field_, get_field ) |> Conflict.single [] )
            |> Expect.equal
              ( "field"
              , { field = model.field |> Conflict.toConflict "conflict", prop = field_, last = "conflict" }
              , { errors = ["conflict"], isModified = True }
              )
      ]
    , describe "compose"
      [ test "should return View" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            get_fieldA = .fieldA

            model =
              { fieldA = "fieldA" |> Conflict.init signature ""
              }

            view a =
              { fieldA = a
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first =
                  { fieldA = "fieldA"
                  } |> Just
                , last =
                  { fieldA = "conflict"
                  }
                }
              }
          in
            Conflict.compose view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA |> Conflict.toConflict "conflict", prop = fieldA_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
                }
              }

      , test "should return View with not conflict" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            get_fieldA = .fieldA

            model =
              { fieldA = "fieldA" |> Conflict.init signature ""
              }

            view a =
              { fieldA = a
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first =
                  { fieldA = "fieldA"
                  } |> Just
                , last =
                  { fieldA = "fieldA"
                  }
                }
              }
          in
            Conflict.compose view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = False
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = True }
                  )
                }
              }

      , test "should return View with not modified" <|
        \_ ->
          let
            signature = "info"

            fieldA_ = Form.prop .fieldA (\v m -> { m | fieldA = v })
            get_fieldA = .fieldA

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              }

            view a =
              { fieldA = a
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first =
                  { fieldA = "fieldA"
                  } |> Just
                , last =
                  { fieldA = "fieldA"
                  }
                }
              }
          in
            Conflict.compose view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = False
              , hasModified = False
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature ""
              }

            view a b =
              { fieldA = a
              , fieldB = b
              }

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldB = "conflict" }
                }
              }
          in
            Conflict.compose2 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB |> Conflict.toConflict "conflict", prop = fieldB_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature ""
              }

            view a b c =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              }

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldC = "conflict" }
                }
              }
          in
            Conflict.compose3 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC |> Conflict.toConflict "conflict", prop = fieldC_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature ""
              }

            view a b c d =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              }

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldD = "conflict" }
                }
              }
          in
            Conflict.compose4 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD |> Conflict.toConflict "conflict", prop = fieldD_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature ""
              }

            view a b c d e =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              }

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldE = "conflict" }
                }
              }
          in
            Conflict.compose5 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE |> Conflict.toConflict "conflict", prop = fieldE_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature ""
              }

            view a b c d e f =
              { fieldA = a
              , fieldB = b
              , fieldC = c
              , fieldD = d
              , fieldE = e
              , fieldF = f
              }

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldF = "conflict" }
                }
              }
          in
            Conflict.compose6 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF |> Conflict.toConflict "conflict", prop = fieldF_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldG = "conflict" }
                }
              }
          in
            Conflict.compose7 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG |> Conflict.toConflict "conflict", prop = fieldG_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldH = "conflict" }
                }
              }
          in
            Conflict.compose8 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH |> Conflict.toConflict "conflict", prop = fieldH_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH
            get_fieldI = .fieldI

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature "fieldH"
              , fieldI = "fieldI" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              , fieldI = "fieldI"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldI = "conflict" }
                }
              }
          in
            Conflict.compose9 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldI_, get_fieldI ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH, prop = fieldH_, last = "fieldH" }
                  , { errors = [], isModified = False }
                  )
                , fieldI =
                  ( "fieldI"
                  , { field = model.fieldI |> Conflict.toConflict "conflict", prop = fieldI_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH
            get_fieldI = .fieldI
            get_fieldJ = .fieldJ

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature "fieldH"
              , fieldI = "fieldI" |> Conflict.init signature "fieldI"
              , fieldJ = "fieldJ" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              , fieldI = "fieldI"
              , fieldJ = "fieldJ"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldJ = "conflict" }
                }
              }
          in
            Conflict.compose10 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldI_, get_fieldI ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldJ_, get_fieldJ ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH, prop = fieldH_, last = "fieldH" }
                  , { errors = [], isModified = False }
                  )
                , fieldI =
                  ( "fieldI"
                  , { field = model.fieldI, prop = fieldI_, last = "fieldI" }
                  , { errors = [], isModified = False }
                  )
                , fieldJ =
                  ( "fieldJ"
                  , { field = model.fieldJ |> Conflict.toConflict "conflict", prop = fieldJ_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH
            get_fieldI = .fieldI
            get_fieldJ = .fieldJ
            get_fieldK = .fieldK

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature "fieldH"
              , fieldI = "fieldI" |> Conflict.init signature "fieldI"
              , fieldJ = "fieldJ" |> Conflict.init signature "fieldJ"
              , fieldK = "fieldK" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              , fieldI = "fieldI"
              , fieldJ = "fieldJ"
              , fieldK = "fieldK"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldK = "conflict" }
                }
              }
          in
            Conflict.compose11 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldI_, get_fieldI ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldJ_, get_fieldJ ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldK_, get_fieldK ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH, prop = fieldH_, last = "fieldH" }
                  , { errors = [], isModified = False }
                  )
                , fieldI =
                  ( "fieldI"
                  , { field = model.fieldI, prop = fieldI_, last = "fieldI" }
                  , { errors = [], isModified = False }
                  )
                , fieldJ =
                  ( "fieldJ"
                  , { field = model.fieldJ, prop = fieldJ_, last = "fieldJ" }
                  , { errors = [], isModified = False }
                  )
                , fieldK =
                  ( "fieldK"
                  , { field = model.fieldK |> Conflict.toConflict "conflict", prop = fieldK_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH
            get_fieldI = .fieldI
            get_fieldJ = .fieldJ
            get_fieldK = .fieldK
            get_fieldL = .fieldL

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature "fieldH"
              , fieldI = "fieldI" |> Conflict.init signature "fieldI"
              , fieldJ = "fieldJ" |> Conflict.init signature "fieldJ"
              , fieldK = "fieldK" |> Conflict.init signature "fieldK"
              , fieldL = "fieldL" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              , fieldI = "fieldI"
              , fieldJ = "fieldJ"
              , fieldK = "fieldK"
              , fieldL = "fieldL"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldL = "conflict" }
                }
              }
          in
            Conflict.compose12 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldI_, get_fieldI ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldJ_, get_fieldJ ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldK_, get_fieldK ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldL_, get_fieldL ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH, prop = fieldH_, last = "fieldH" }
                  , { errors = [], isModified = False }
                  )
                , fieldI =
                  ( "fieldI"
                  , { field = model.fieldI, prop = fieldI_, last = "fieldI" }
                  , { errors = [], isModified = False }
                  )
                , fieldJ =
                  ( "fieldJ"
                  , { field = model.fieldJ, prop = fieldJ_, last = "fieldJ" }
                  , { errors = [], isModified = False }
                  )
                , fieldK =
                  ( "fieldK"
                  , { field = model.fieldK, prop = fieldK_, last = "fieldK" }
                  , { errors = [], isModified = False }
                  )
                , fieldL =
                  ( "fieldL"
                  , { field = model.fieldL |> Conflict.toConflict "conflict", prop = fieldL_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH
            get_fieldI = .fieldI
            get_fieldJ = .fieldJ
            get_fieldK = .fieldK
            get_fieldL = .fieldL
            get_fieldM = .fieldM

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature "fieldH"
              , fieldI = "fieldI" |> Conflict.init signature "fieldI"
              , fieldJ = "fieldJ" |> Conflict.init signature "fieldJ"
              , fieldK = "fieldK" |> Conflict.init signature "fieldK"
              , fieldL = "fieldL" |> Conflict.init signature "fieldL"
              , fieldM = "fieldM" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              , fieldI = "fieldI"
              , fieldJ = "fieldJ"
              , fieldK = "fieldK"
              , fieldL = "fieldL"
              , fieldM = "fieldM"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldM = "conflict" }
                }
              }
          in
            Conflict.compose13 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldI_, get_fieldI ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldJ_, get_fieldJ ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldK_, get_fieldK ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldL_, get_fieldL ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldM_, get_fieldM ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH, prop = fieldH_, last = "fieldH" }
                  , { errors = [], isModified = False }
                  )
                , fieldI =
                  ( "fieldI"
                  , { field = model.fieldI, prop = fieldI_, last = "fieldI" }
                  , { errors = [], isModified = False }
                  )
                , fieldJ =
                  ( "fieldJ"
                  , { field = model.fieldJ, prop = fieldJ_, last = "fieldJ" }
                  , { errors = [], isModified = False }
                  )
                , fieldK =
                  ( "fieldK"
                  , { field = model.fieldK, prop = fieldK_, last = "fieldK" }
                  , { errors = [], isModified = False }
                  )
                , fieldL =
                  ( "fieldL"
                  , { field = model.fieldL, prop = fieldL_, last = "fieldL" }
                  , { errors = [], isModified = False }
                  )
                , fieldM =
                  ( "fieldM"
                  , { field = model.fieldM |> Conflict.toConflict "conflict", prop = fieldM_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH
            get_fieldI = .fieldI
            get_fieldJ = .fieldJ
            get_fieldK = .fieldK
            get_fieldL = .fieldL
            get_fieldM = .fieldM
            get_fieldN = .fieldN

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature "fieldH"
              , fieldI = "fieldI" |> Conflict.init signature "fieldI"
              , fieldJ = "fieldJ" |> Conflict.init signature "fieldJ"
              , fieldK = "fieldK" |> Conflict.init signature "fieldK"
              , fieldL = "fieldL" |> Conflict.init signature "fieldL"
              , fieldM = "fieldM" |> Conflict.init signature "fieldM"
              , fieldN = "fieldN" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              , fieldI = "fieldI"
              , fieldJ = "fieldJ"
              , fieldK = "fieldK"
              , fieldL = "fieldL"
              , fieldM = "fieldM"
              , fieldN = "fieldN"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldN = "conflict" }
                }
              }
          in
            Conflict.compose14 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldI_, get_fieldI ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldJ_, get_fieldJ ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldK_, get_fieldK ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldL_, get_fieldL ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldM_, get_fieldM ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldN_, get_fieldN ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH, prop = fieldH_, last = "fieldH" }
                  , { errors = [], isModified = False }
                  )
                , fieldI =
                  ( "fieldI"
                  , { field = model.fieldI, prop = fieldI_, last = "fieldI" }
                  , { errors = [], isModified = False }
                  )
                , fieldJ =
                  ( "fieldJ"
                  , { field = model.fieldJ, prop = fieldJ_, last = "fieldJ" }
                  , { errors = [], isModified = False }
                  )
                , fieldK =
                  ( "fieldK"
                  , { field = model.fieldK, prop = fieldK_, last = "fieldK" }
                  , { errors = [], isModified = False }
                  )
                , fieldL =
                  ( "fieldL"
                  , { field = model.fieldL, prop = fieldL_, last = "fieldL" }
                  , { errors = [], isModified = False }
                  )
                , fieldM =
                  ( "fieldM"
                  , { field = model.fieldM, prop = fieldM_, last = "fieldM" }
                  , { errors = [], isModified = False }
                  )
                , fieldN =
                  ( "fieldN"
                  , { field = model.fieldN |> Conflict.toConflict "conflict", prop = fieldN_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH
            get_fieldI = .fieldI
            get_fieldJ = .fieldJ
            get_fieldK = .fieldK
            get_fieldL = .fieldL
            get_fieldM = .fieldM
            get_fieldN = .fieldN
            get_fieldO = .fieldO

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature "fieldH"
              , fieldI = "fieldI" |> Conflict.init signature "fieldI"
              , fieldJ = "fieldJ" |> Conflict.init signature "fieldJ"
              , fieldK = "fieldK" |> Conflict.init signature "fieldK"
              , fieldL = "fieldL" |> Conflict.init signature "fieldL"
              , fieldM = "fieldM" |> Conflict.init signature "fieldM"
              , fieldN = "fieldN" |> Conflict.init signature "fieldN"
              , fieldO = "fieldO" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              , fieldI = "fieldI"
              , fieldJ = "fieldJ"
              , fieldK = "fieldK"
              , fieldL = "fieldL"
              , fieldM = "fieldM"
              , fieldN = "fieldN"
              , fieldO = "fieldO"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldO = "conflict" }
                }
              }
          in
            Conflict.compose15 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldI_, get_fieldI ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldJ_, get_fieldJ ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldK_, get_fieldK ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldL_, get_fieldL ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldM_, get_fieldM ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldN_, get_fieldN ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldO_, get_fieldO ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH, prop = fieldH_, last = "fieldH" }
                  , { errors = [], isModified = False }
                  )
                , fieldI =
                  ( "fieldI"
                  , { field = model.fieldI, prop = fieldI_, last = "fieldI" }
                  , { errors = [], isModified = False }
                  )
                , fieldJ =
                  ( "fieldJ"
                  , { field = model.fieldJ, prop = fieldJ_, last = "fieldJ" }
                  , { errors = [], isModified = False }
                  )
                , fieldK =
                  ( "fieldK"
                  , { field = model.fieldK, prop = fieldK_, last = "fieldK" }
                  , { errors = [], isModified = False }
                  )
                , fieldL =
                  ( "fieldL"
                  , { field = model.fieldL, prop = fieldL_, last = "fieldL" }
                  , { errors = [], isModified = False }
                  )
                , fieldM =
                  ( "fieldM"
                  , { field = model.fieldM, prop = fieldM_, last = "fieldM" }
                  , { errors = [], isModified = False }
                  )
                , fieldN =
                  ( "fieldN"
                  , { field = model.fieldN, prop = fieldN_, last = "fieldN" }
                  , { errors = [], isModified = False }
                  )
                , fieldO =
                  ( "fieldO"
                  , { field = model.fieldO |> Conflict.toConflict "conflict", prop = fieldO_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
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
            get_fieldA = .fieldA
            get_fieldB = .fieldB
            get_fieldC = .fieldC
            get_fieldD = .fieldD
            get_fieldE = .fieldE
            get_fieldF = .fieldF
            get_fieldG = .fieldG
            get_fieldH = .fieldH
            get_fieldI = .fieldI
            get_fieldJ = .fieldJ
            get_fieldK = .fieldK
            get_fieldL = .fieldL
            get_fieldM = .fieldM
            get_fieldN = .fieldN
            get_fieldO = .fieldO
            get_fieldP = .fieldP

            model =
              { fieldA = "fieldA" |> Conflict.init signature "fieldA"
              , fieldB = "fieldB" |> Conflict.init signature "fieldB"
              , fieldC = "fieldC" |> Conflict.init signature "fieldC"
              , fieldD = "fieldD" |> Conflict.init signature "fieldD"
              , fieldE = "fieldE" |> Conflict.init signature "fieldE"
              , fieldF = "fieldF" |> Conflict.init signature "fieldF"
              , fieldG = "fieldG" |> Conflict.init signature "fieldG"
              , fieldH = "fieldH" |> Conflict.init signature "fieldH"
              , fieldI = "fieldI" |> Conflict.init signature "fieldI"
              , fieldJ = "fieldJ" |> Conflict.init signature "fieldJ"
              , fieldK = "fieldK" |> Conflict.init signature "fieldK"
              , fieldL = "fieldL" |> Conflict.init signature "fieldL"
              , fieldM = "fieldM" |> Conflict.init signature "fieldM"
              , fieldN = "fieldN" |> Conflict.init signature "fieldN"
              , fieldO = "fieldO" |> Conflict.init signature "fieldO"
              , fieldP = "fieldP" |> Conflict.init signature ""
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

            response =
              { fieldA = "fieldA"
              , fieldB = "fieldB"
              , fieldC = "fieldC"
              , fieldD = "fieldD"
              , fieldE = "fieldE"
              , fieldF = "fieldF"
              , fieldG = "fieldG"
              , fieldH = "fieldH"
              , fieldI = "fieldI"
              , fieldJ = "fieldJ"
              , fieldK = "fieldK"
              , fieldL = "fieldL"
              , fieldM = "fieldM"
              , fieldN = "fieldN"
              , fieldO = "fieldO"
              , fieldP = "fieldP"
              }

            opts =
              { error = "conflict"
              , form  = model
              , response =
                { first = response |> Just
                , last  = { response | fieldP = "conflict" }
                }
              }
          in
            Conflict.compose16 view
              ( opts |> ( ( fieldA_, get_fieldA ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldB_, get_fieldB ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldC_, get_fieldC ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldD_, get_fieldD ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldE_, get_fieldE ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldF_, get_fieldF ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldG_, get_fieldG ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldH_, get_fieldH ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldI_, get_fieldI ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldJ_, get_fieldJ ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldK_, get_fieldK ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldL_, get_fieldL ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldM_, get_fieldM ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldN_, get_fieldN ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldO_, get_fieldO ) |> Conflict.single [] ) )
              ( opts |> ( ( fieldP_, get_fieldP ) |> Conflict.single [] ) )
            |> Expect.equal
              { hasError = True
              , hasModified = True
              , form =
                { fieldA =
                  ( "fieldA"
                  , { field = model.fieldA, prop = fieldA_, last = "fieldA" }
                  , { errors = [], isModified = False }
                  )
                , fieldB =
                  ( "fieldB"
                  , { field = model.fieldB, prop = fieldB_, last = "fieldB" }
                  , { errors = [], isModified = False }
                  )
                , fieldC =
                  ( "fieldC"
                  , { field = model.fieldC, prop = fieldC_, last = "fieldC" }
                  , { errors = [], isModified = False }
                  )
                , fieldD =
                  ( "fieldD"
                  , { field = model.fieldD, prop = fieldD_, last = "fieldD" }
                  , { errors = [], isModified = False }
                  )
                , fieldE =
                  ( "fieldE"
                  , { field = model.fieldE, prop = fieldE_, last = "fieldE" }
                  , { errors = [], isModified = False }
                  )
                , fieldF =
                  ( "fieldF"
                  , { field = model.fieldF, prop = fieldF_, last = "fieldF" }
                  , { errors = [], isModified = False }
                  )
                , fieldG =
                  ( "fieldG"
                  , { field = model.fieldG, prop = fieldG_, last = "fieldG" }
                  , { errors = [], isModified = False }
                  )
                , fieldH =
                  ( "fieldH"
                  , { field = model.fieldH, prop = fieldH_, last = "fieldH" }
                  , { errors = [], isModified = False }
                  )
                , fieldI =
                  ( "fieldI"
                  , { field = model.fieldI, prop = fieldI_, last = "fieldI" }
                  , { errors = [], isModified = False }
                  )
                , fieldJ =
                  ( "fieldJ"
                  , { field = model.fieldJ, prop = fieldJ_, last = "fieldJ" }
                  , { errors = [], isModified = False }
                  )
                , fieldK =
                  ( "fieldK"
                  , { field = model.fieldK, prop = fieldK_, last = "fieldK" }
                  , { errors = [], isModified = False }
                  )
                , fieldL =
                  ( "fieldL"
                  , { field = model.fieldL, prop = fieldL_, last = "fieldL" }
                  , { errors = [], isModified = False }
                  )
                , fieldM =
                  ( "fieldM"
                  , { field = model.fieldM, prop = fieldM_, last = "fieldM" }
                  , { errors = [], isModified = False }
                  )
                , fieldN =
                  ( "fieldN"
                  , { field = model.fieldN, prop = fieldN_, last = "fieldN" }
                  , { errors = [], isModified = False }
                  )
                , fieldO =
                  ( "fieldO"
                  , { field = model.fieldO, prop = fieldO_, last = "fieldO" }
                  , { errors = [], isModified = False }
                  )
                , fieldP =
                  ( "fieldP"
                  , { field = model.fieldP |> Conflict.toConflict "conflict", prop = fieldP_, last = "conflict" }
                  , { errors = ["conflict"], isModified = True }
                  )
                }
              }
      ]
    ]
