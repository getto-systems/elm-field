module Getto.Field.EditTest exposing (..)
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict
import Getto.Field.Edit as Edit

import Json.Encode as Encode
import Json.Decode as Decode

import Set

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

{-
  , cancel
  , encode
  , decode
-}

suite : Test
suite =
  describe "Edit"
    [ describe "view"
      [ test "should return static view" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John"
              }

            response = { name = "John" }
            form = fields |> Edit.form

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = response
              , isConflict  = False
              , isDifferent = \first last -> False
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = True
              , state = Edit.Same
              , form =
                { name =
                  ( "name"
                  , { field = fields.name, prop = name_, last = "John" }
                  , { errors = [], isModified = False }
                  )
                }
              }

      , test "should return edit view" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John"
              }

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response)

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = response
              , isConflict  = False
              , isDifferent = \first last -> False
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = False
              , state = Edit.Same
              , form =
                { name =
                  ( "name"
                  , { field = fields.name, prop = name_, last = "John" }
                  , { errors = [], isModified = False }
                  )
                }
              }

      , test "should return static view with cancel state" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John"
              }

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response) |> Edit.cancel

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = response
              , isConflict  = False
              , isDifferent = \first last -> False
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = True
              , state = Edit.Same
              , form =
                { name =
                  ( "name"
                  , { field = fields.name, prop = name_, last = "John" }
                  , { errors = [], isModified = False }
                  )
                }
              }

      , test "should return edit view with modified" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John Doe"
              }

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response)

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = response
              , isConflict  = False
              , isDifferent = \first last -> False
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = False
              , state = Edit.HasModified
              , form =
                { name =
                  ( "name"
                  , { field = fields.name, prop = name_, last = "John" }
                  , { errors = [], isModified = True }
                  )
                }
              }

      , test "should return edit view with error" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature ""
              }

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response)

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = response
              , isConflict  = False
              , isDifferent = \first last -> False
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = False
              , state = Edit.HasError
              , form =
                { name =
                  ( "name"
                  , { field = fields.name, prop = name_, last = "John" }
                  , { errors = ["blank"], isModified = True }
                  )
                }
              }

      , test "should return edit view with conflict" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John-Doe"
              }

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response)

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = { response | name = "John Doe" }
              , isConflict  = False
              , isDifferent = \first last -> False
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = False
              , state = Edit.HasError
              , form =
                { name =
                  ( "name"
                  , { field = fields.name |> Conflict.toConflict "John Doe", prop = name_, last = "John Doe" }
                  , { errors = ["conflict"], isModified = True }
                  )
                }
              }

      , test "should return edit view with commit state" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John Doe"
              }

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response) |> Edit.commit

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = response
              , isConflict  = False
              , isDifferent = \first last -> False
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = False
              , state = Edit.HasModified
              , form =
                { name =
                  ( "name"
                  , { field = fields.name, prop = name_, last = "John" }
                  , { errors = [], isModified = True }
                  )
                }
              }

      , test "should return static view with commit state and different response" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John Doe"
              }

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response) |> Edit.commit

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = response
              , isConflict  = False
              , isDifferent = \first last -> True
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = True
              , state = Edit.HasModified
              , form =
                { name =
                  ( "name"
                  , { field = fields.name, prop = name_, last = "John" }
                  , { errors = [], isModified = True }
                  )
                }
              }

      , test "should return edit view with changed state and different response" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John Doe"
              }

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response) |> Edit.commit |> Edit.change

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))
            get_name = .name

            blank = Validate.blank "blank"

            (isStatic,options) =
              { error       = "conflict"
              , form        = form
              , last        = response
              , isConflict  = False
              , isDifferent = \first last -> True
              }
              |> Edit.options

            view a =
              { name = a
              }
          in
            ( isStatic
            , Conflict.compose view
              ( options |> ( ( name_, get_name ) |> Conflict.single [ fields.name |> blank ] ) )
            )
            |> Edit.view
            |> Expect.equal
              { isStatic = False
              , state = Edit.HasModified
              , form =
                { name =
                  ( "name"
                  , { field = fields.name, prop = name_, last = "John" }
                  , { errors = [], isModified = True }
                  )
                }
              }
      ]
    , describe "encode/decode"
      [ test "should encode static form" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John"
              }

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))

            response = { name = "John" }
            form = fields |> Edit.form

            view a =
              { name = a
              }

            responseDecoder = Decode.map view
              ( Decode.at ["name"] Decode.string )

            decodeForm value =
              Form.setIf name_
                ( value |> Decode.decodeValue (Decode.at ["name"] Decode.string) |> Result.toMaybe )

            encodeResponse res =
              [ ( "name", res.name |> Encode.string )
              ] |> Encode.object

            encodeModel model =
              [ model.name |> Field.name_value |> Tuple.mapSecond Encode.string
              ] |> Encode.object
          in
            { name = "name" |> Conflict.init signature ""
            }
            |> Edit.form
            |> Edit.decode responseDecoder decodeForm
              ( form |> Edit.encode encodeResponse encodeModel )
            |> Expect.equal
              ( { name = "name" |> Conflict.init signature ""
                }
                |> Edit.form
              )

      , test "should encode edit form" <|
        \_ ->
          let
            signature = "info"

            fields =
              { name = "name" |> Conflict.init signature "John"
              }

            name_ = Form.prop (Edit.fields >> .name) (\v -> Edit.update (\m -> { m | name = v }))

            response = { name = "John" }
            form = fields |> Edit.form |> Edit.edit (\res -> identity) (Just response)

            view a =
              { name = a
              }

            responseDecoder = Decode.map view
              ( Decode.at ["name"] Decode.string )

            decodeForm value =
              Form.setIf name_
                ( value |> Decode.decodeValue (Decode.at ["name"] Decode.string) |> Result.toMaybe )

            encodeResponse res =
              [ ( "name", res.name |> Encode.string )
              ] |> Encode.object

            encodeModel model =
              [ model.name |> Field.name_value |> Tuple.mapSecond Encode.string
              ] |> Encode.object
          in
            { name = "name" |> Conflict.init signature ""
            }
            |> Edit.form
            |> Edit.decode responseDecoder decodeForm
              ( form |> Edit.encode encodeResponse encodeModel )
            |> Expect.equal
              ( { name = "name" |> Conflict.init signature "John"
                }
                |> Edit.form |> Edit.edit (\res -> identity) (Just response)
              )
      ]
    ]
