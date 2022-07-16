module Form.FieldState exposing (..)

type FieldValue
    = FVString String
    | FVBool Bool

type ErrorState error 
    = Error error 
    | Loading 
    | NoError 

type alias InternalFieldState error = 
    { value: Maybe FieldValue
    , errorState: ErrorState error 
    }
    

getFieldValue : InternalFieldState error -> Maybe FieldValue 
getFieldValue = 
    .value

getString : FieldValue -> Maybe String 
getString fieldValue = 
    case fieldValue of 
        FVString s -> 
            Just s 

        _ -> 
            Nothing 

getBool : FieldValue -> Maybe Bool 
getBool fieldValue = 
    case fieldValue of 
        FVBool b -> 
            Just b 

        _ -> 
            Nothing 



