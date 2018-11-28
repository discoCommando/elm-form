module Form.Validation.Types exposing (..)

import Form.Map as Map exposing (Map)
import Form.Types exposing (FailState, Field, SuccessState)


type alias ValidationResult error field output =
    { fields : Map field (ValidationResultCell error field)
    , result : Maybe output
    }


type alias ValidationResultCell error field =
    { indexOfList : Map field Int
    , error : Maybe error
    }


fromFailState : FailState error field -> ValidationResult error field output
fromFailState failState =
    { fields = failState.fields
    , result = Nothing
    }


fromSuccessState : SuccessState field output -> ValidationResult error field output
fromSuccessState successState =
    { fields = successState.fields |> Map.mapValue (\successCell -> ValidationResultCell successCell.indexOfList Nothing)
    , result = Just successState.output
    }
