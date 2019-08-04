module Example exposing (SingleFieldTest(..), SingleFieldTestOutput1, singleFieldTest1)

import Expect exposing (Expectation)
import Form
import Form.Transaction
import Form.Validation exposing (andMap, fromString, string, succeed, int)
import Fuzz exposing (Fuzzer, list)
import Test exposing (..)



-- Single Field Tests


type SingleFieldTest
    = SimpleTestStringField (Form.Value String)


type alias SingleFieldTestOutput1 =
    { simpleTestStringField : String }


singleFieldTest1 : Test
singleFieldTest1 =
    let
        -- simpleValidation : Form.Validation () SingleFieldTest SimpleTest1Output
        simpleValidation =
            succeed SingleFieldTestOutput1
                |> andMap (fromString SimpleTestStringValue String)
    in
    describe "Form with one string field"
        [ describe "Not setting a string" <|
            let
                form =
                    Form.form simpleValidation
            in
            [ test "Value should be empty" <|
                \_ ->
                    form
                        |> Form.get SimpleTestStringField
                        |> Expect.equal ""
            , test "All fields are required by default" <|
                \_ ->
                    form
                        |> Form.getError SimpleTestStringField
                        |> Expect.equal (Just ())
            ]
        , describe "Setting a string" <|
            let
                form value =
                    Form.form simpleValidation
                        |> Form.Transaction.save (Form.Transaction.setString SimpleTestStringField value)
            in
            [ fuzz Fuzz.string "Value of the field is the same as the one set" <|
                \value ->
                    form value
                        |> Form.get SimpleTestStringField
                        |> Expect.equal value
            , fuzz Fuzz.string "Check for error" <|
                \value ->
                    let
                        expectedError =
                            if value == "" then
                                Just ()

                            else
                                Nothing
                    in
                    form value
                        |> Form.getError SimpleTestStringField
                        |> Expect.equal expectedError
            , fuzz Fuzz.string "Check for output" <|
                \value ->
                    let
                        expectedOutput =
                            if value == "" then
                                Nothing

                            else
                                Just { simpleTestStringField = value }
                    in
                    form value
                        |> Form.getOutput
                        |> Expect.equal expectedOutput
            ]
        ]


type alias SingleFieldTestOutput2 =
    { simpleTestStringField : Int }


singleFieldTest2 : Test
singleFieldTest2 =
    let
        validation =
            succeed SingleFieldTestOutput2
                |> andMap (fromString SimpleTestStringField int)

        form value =
            Form.form validation
                |> Form.Transaction.save (Form.Transaction.setString SimpleTestStringField value)
    in
    describe "Parsing int"
        [ fuzz Fuzz.string "Check for errors" <|
            \string ->
                let
                    expectedError = 
                        case String.toInt string of 
                            Nothing -> Just ()
                            _ -> Nothing
                in
                form string
                    |> Form.getError SimpleTestStringField
                    |> Expect.equal expectedError
        , fuzz Fuzz.int "Check for output" <|
            \int_ ->
                let
                    expectedOutput =
                        Just { simpleTestStringField = int_ }
                in
                form (String.fromInt int_)
                    |> Form.getOutput
                    |> Expect.equal expectedOutput
        ]
