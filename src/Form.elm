module Form exposing
    ( Form
    , View(..)
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
import Html exposing (Html)
import Index.FieldIndex as FieldIndex exposing (FieldIndex)
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex as UniqueIndex exposing (UniqueIndex)


type View error field msg
    = VI_STRING (Field.Value String -> field) (String -> Maybe error -> Html msg)
    | VI_HTML (Html msg)
    | VI_VIEW String (List (Html.Attribute msg)) (List (View error field msg))
    | VI_REMOVELASTROW field (Maybe UniqueIndex -> Html msg)
    | VI_INLIST field (List UniqueIndex -> View error field msg)
    | VI_LAZY (() -> View error field msg)


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
