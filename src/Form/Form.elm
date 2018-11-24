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
    let
        ( fields, output ) =
            Form.Validation.validate validation Map.empty
    in
    { fields = fields
    , submitted = False
    , validation = validation
    , output = output
    }
