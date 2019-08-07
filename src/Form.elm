module Form exposing
    ( Form
    , View
    , Validation
    , form
    , getOutput
    , validate
    )

-- COMPOSABLE
-- TYPE SAFE

import Form.Field as Field
import Form.FieldState exposing (FieldState)
import Form.Map as Map exposing (Map)
import Form.Validation exposing (Validation, validate_)
import Form.View
import Html exposing (Html)
import Index.FieldIndex as FieldIndex exposing (FieldIndex)
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex as UniqueIndex exposing (UniqueIndex)


type alias View error field msg =
    Form.View.View error field msg

type alias Validation error field output = 
    Form.Validation.Validation error field output 


type alias Form field error output =
    Form.Validation.Form field error output


form : Validation field error output -> Form field error output
form validation =
    { fieldIndexes = Map.empty
    , listIndexes = FieldIndexDict.empty
    , values = FieldIndexDict.empty
    , submitted = False
    , validation = validation
    , output = Nothing
    , fieldIndexToUse = FieldIndex.create
    , uniqueIndexToUse = UniqueIndex.create
    , counter = 0
    }
        |> validate


validate : Form error field output -> Form error field output
validate =
    validate_


getOutput : Form error field output -> Maybe output
getOutput form_ =
    form_.output
