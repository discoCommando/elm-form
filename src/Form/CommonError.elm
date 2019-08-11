module Form.CommonError exposing (CommonError(..), custom)


type CommonError
    = NoInput
    | Custom String


custom : String -> CommonError
custom =
    Custom
