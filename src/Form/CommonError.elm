module Form.CommonError exposing (CommonError(..), custom)


type CommonError
    = NotFound
    | NotEditedYet
    | Custom String


custom : String -> CommonError
custom =
    Custom
