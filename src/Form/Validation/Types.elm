module Form.Validation.Types exposing (..)

import Form.Map exposing (Map)
import Form.Types exposing (Field)


type alias FailState error field =
    { succeeded : List field
    , errors : List ( field, error )
    , notFounds : List field
    }


type Validation error field output
    = STR (Field String -> field) (Maybe String -> Validation error field output)
    | LIST (Int -> Validation error field output)
    | FAIL (FailState error field)
    | SUCCESS (List field) output


type alias ValidationResult error field output =
    { fields : Map field (Maybe error)
    , result : Maybe output
    }
