module Form.FieldState exposing (..)

-- type alias Transaction error field output =
--     { newFields : Fields error field
--     , initialForm : Form error field output
--     }


type FieldValue
    = FVString String
    | FVBool Bool
    | FVEmpty -- should happen only once there is an error in validation


type alias FieldState error =
    { error : Maybe error
    , value : FieldValue
    }


stringValue : String -> FieldValue
stringValue =
    FVString


asString : FieldValue -> Maybe String
asString fieldValue =
    case fieldValue of
        FVString s ->
            Just s

        _ ->
            Nothing


boolValue : Bool -> FieldValue
boolValue =
    FVBool


asBool : FieldValue -> Maybe Bool
asBool fieldValue =
    case fieldValue of
        FVBool b ->
            Just b

        _ ->
            Nothing


newFieldState : FieldValue -> FieldState error
newFieldState fieldValue =
    { error = Nothing
    , value = fieldValue
    }
