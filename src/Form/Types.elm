module Form.Types exposing (..)

import Form.Map as Map exposing (Map)


type Field a
    = Field


type FieldNested a
    = FieldNested
    | FieldNestedNotOpaque a


type alias Form error field output =
    { fields : Fields error field
    , submitted : Bool
    , validation : Validation error field output
    , output : Maybe output
    }


type alias Fields error field =
    Map field (FieldState error)


type alias Transaction error field output =
    { newFields : Fields error field
    , initialForm : Form error field output
    }


type FieldValue
    = FVString String
    | FVBool Bool
    | FVList (List FieldValue)



-- | FVNested (Fields error field)


type alias FieldState error =
    { error : Maybe error
    , value : FieldValue
    }


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


listValue : List FieldValue -> FieldValue
listValue =
    FVList


asList : FieldValue -> Maybe (List FieldValue)
asList fieldValue =
    case fieldValue of
        FVList l ->
            Just l

        _ ->
            Nothing



-- nestedValue : Fields error field -> FieldValue
-- nestedValue = FVNested
-- asNested : FieldValue -> Maybe (Fields error field)
-- asNested fieldValue =
--     case fieldValue of
--         FVNested fs ->
--             Just fs
--         _ ->
--             Nothing


newFieldState : FieldValue -> FieldState error
newFieldState fieldValue =
    { error = Nothing
    , value = fieldValue
    }


field : (Field a -> field) -> field
field fieldF =
    fieldF Field


fieldNested : (FieldNested a -> field) -> field
fieldNested fieldF =
    fieldF FieldNested


fieldNestedNotOpaque : (FieldNested a -> field) -> a -> field
fieldNestedNotOpaque fieldF a =
    fieldF (FieldNestedNotOpaque a)


setError : error -> FieldState error -> FieldState error
setError error fieldState =
    { fieldState | error = Just error }


removeError : FieldState error -> FieldState error
removeError fieldState =
    { fieldState | error = Nothing }



-- mapFieldState : (a -> b) -> FieldState error a -> FieldState error b
-- mapFieldState f fieldState =
--     { error = fieldState.error
--     , value = fieldState.value |> mapFieldValue f
--     }
-- mapFieldValue : (a -> b) -> FieldValue error a -> FieldValue error b
-- mapFieldValue f fieldValue =
--     case fieldValue of
--         FVString s ->
--             FVString s
--         FVBool b ->
--             FVBool b
--         FVList list ->
--             FVList (list |> List.map (mapFieldValue f))
--         FVNested nested ->
--             FVNested (nested |> mapFields f)


mapFields : (a -> b) -> Fields error a -> Fields error b
mapFields f fields =
    fields
        |> Map.mapKey f
