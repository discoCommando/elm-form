module Form.Form exposing (..)

-- COMPOSABLE
-- TYPE SAFE

import Form.Map as Map
import Form.Types exposing (..)
import Form.Validation
import Html.Attributes
import Html.Events exposing (onCheck)


-- initialFields =
--     [Field1 ""]
-- INIT


form : Validation field error output -> Form field error output
form validation =
    { fields = Map.empty
    , submitted = False
    , validation = validation
    , output = Nothing
    }
        |> Form.Validation.validateForm validation
