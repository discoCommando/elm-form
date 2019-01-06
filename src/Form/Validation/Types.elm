module Form.Validation.Types exposing (..)

import Form.Types exposing (FailState, Field, SuccessState)


type ValidationResult error field output
    = Failed (FailState error field)
    | Succeeded (SuccessState output)


type alias ValidationResultCell error =
    { error : Maybe error
    }


fromFailState : FailState error field -> ValidationResult error field output
fromFailState =
    Failed


fromSuccessState : SuccessState output -> ValidationResult error field output
fromSuccessState =
    Succeeded
