module Form exposing
    ( Form
    , Submitted
    , Validation
    , View
    , form
    , getOutput
    , validate
    )

-- COMPOSABLE
-- TYPE SAFE

import Form.Field as Field
import Form.Map as Map exposing (Map)
import Form.Type
import Form.Validation exposing (Validation, validate_)
import Form.View2
import Html exposing (Html)
import Index.FieldIndex as FieldIndex exposing (FieldIndex)
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex as UniqueIndex exposing (UniqueIndex)


type alias View error field msg =
    Form.View2.View error field msg


type alias Validation error field output =
    Form.Validation.Validation error field output


type alias Form field error output =
    Form.View2.Form field error output


type alias Submitted =
    Form.View2.Submitted


form : Validation field error output -> Form field error output
form validation =
    { fieldIndexes = Map.empty
    , listIndexes = FieldIndexDict.empty
    , values = FieldIndexDict.empty
    , submitted = Form.View2.NotSubmitted
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
