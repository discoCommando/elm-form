module Form.Validation.Types exposing (..)

import Form.Map exposing (Map)
import Form.Types exposing (Field)


type alias ValidationResult error field output =
    { fields : Map field (Maybe error)
    , result : Maybe output
    , toRemove : List field
    }
