module Form.Form exposing (..)

-- COMPOSABLE
-- TYPE SAFE

import Form.FieldIndex
import Form.Map as Map
import Form.Types exposing (..)
import Form.UniqueIndex
import Form.Validation
import Form.Values
import Html.Attributes
import Html.Events exposing (onCheck)


-- initialFields =
--     [Field1 ""]
-- INIT


form : Validation field error output -> Form field error output
form validation =
    { fieldIndexes = Map.empty
    , listIndexes = Form.Values.empty
    , values = Form.Values.empty
    , submitted = False
    , validation = validation
    , output = Nothing
    , fieldIndexToUse = Form.FieldIndex.create
    , uniqueIndexToUse = Form.UniqueIndex.create
    }
        |> Form.Validation.validate
