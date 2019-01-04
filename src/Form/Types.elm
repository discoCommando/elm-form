module Form.Types exposing (..)

import Form.Map as Map exposing (Map)
import Form.UniqueIndex as UIdx


type Field a
    = Field


type FieldList a
    = OpaqueList
    | WithIndex Int a


type FieldNested a
    = OpaqueNested
    | WithValue a


type alias Form error field output =
    { fields : Fields error field
    , submitted : Bool
    , validation : Validation error field output
    , output : Maybe output
    }


type alias Fields error field =
    Map field (FieldState error field)



-- type alias ListsFields error field =
--     Map field (ListFieldState error)


type alias Transaction error field output =
    { newFields : Fields error field
    , initialForm : Form error field output
    }


type FieldValue
    = FVString String
    | FVBool Bool
    | FVLength Int -- not accessible to anyone



-- | FVList (List FieldValue)
-- | FVNested (Fields error field)
-- type alias ListFieldState error =
--     { error : Maybe error
--     , value : List FieldValue
--     }


type alias FieldState error field =
    { error : Maybe error
    , value : FieldValue
    , indexOfList : Map field Int
    }


type alias IndexOfList field =
    { getter : field
    , index : Int
    }


type alias FailCell error field =
    { indexOfList : Map field Int
    , error : Maybe error
    }


type alias FailState error field =
    { fields : Map field (FailCell error field) }


type alias SuccessCell field =
    { indexOfList : Map field Int
    }


type alias SuccessState field output =
    { output : output
    , fields : Map field (SuccessCell field)
    }


type Validation error field output
    = STR (Field String -> field) (String -> Validation error field output)
    | LIST field (Int -> Validation error field output)
    | FAIL (FailState error field)
    | SUCCESS (SuccessState field output)


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



-- listValue : List FieldValue -> FieldValue
-- listValue =
--     FVList
-- asList : FieldValue -> Maybe (List FieldValue)
-- asList fieldValue =
--     case fieldValue of
--         FVList l ->
--             Just l
--         _ ->
--             Nothing
-- nestedValue : Fields error field -> FieldValue
-- nestedValue = FVNested
-- asNested : FieldValue -> Maybe (Fields error field)
-- asNested fieldValue =
--     case fieldValue of
--         FVNested fs ->
--             Just fs
--         _ ->
--             Nothing


listOpaque : (FieldList a -> field) -> field
listOpaque listF =
    listF OpaqueList


listField : (FieldList a -> field) -> Int -> a -> field
listField listF i a =
    listF (WithIndex i a)


asLength : FieldValue -> Maybe Int
asLength fieldValue =
    case fieldValue of
        FVLength i ->
            Just i

        _ ->
            Nothing


lengthValue : Int -> FieldValue
lengthValue =
    FVLength


getIndex : (FieldList a -> field) -> Fields error field -> Int
getIndex listF fields =
    fields |> Map.get (listOpaque listF) |> Maybe.andThen (.value >> asLength) |> Maybe.withDefault 0


addIndex : (FieldList a -> field) -> Fields error field -> Fields error field
addIndex listF fields =
    fields |> Map.set (listOpaque listF) { value = fields |> getIndex listF |> (+) 1 |> lengthValue, error = Nothing, indexOfList = Map.empty }


newFieldState : FieldValue -> FieldState error field
newFieldState fieldValue =
    { error = Nothing
    , value = fieldValue
    , indexOfList = Map.empty
    }


field : (Field a -> field) -> field
field fieldF =
    fieldF Field


fieldNested : (FieldNested a -> field) -> field
fieldNested fieldF =
    fieldF OpaqueNested


fieldNestedNotOpaque : (FieldNested a -> field) -> a -> field
fieldNestedNotOpaque fieldF a =
    fieldF (WithValue a)


createIndexOfList : (FieldList x -> field) -> Int -> IndexOfList field
createIndexOfList fieldListF i =
    { index = i
    , getter = listOpaque fieldListF
    }


mapIndexOfList : (field1 -> field2) -> IndexOfList field1 -> IndexOfList field2
mapIndexOfList mapf indexOfList =
    { indexOfList | getter = indexOfList.getter |> mapf }


mapFailCell : (field1 -> field2) -> FailCell error field1 -> FailCell error field2
mapFailCell mapf failCell =
    { failCell | indexOfList = failCell.indexOfList |> Map.mapKey mapf }


mapFailState : (field1 -> field2) -> FailState error field1 -> FailState error field2
mapFailState mapf failState =
    { failState | fields = failState.fields |> Map.mapBoth (\key failCell -> ( key |> mapf, mapFailCell mapf failCell )) }


successCellToFailCell : SuccessCell field -> FailCell error field
successCellToFailCell { indexOfList } =
    FailCell indexOfList Nothing


mapSuccessCell : (field1 -> field2) -> SuccessCell field1 -> SuccessCell field2
mapSuccessCell mapf successCell =
    { successCell | indexOfList = successCell.indexOfList |> Map.mapKey mapf }


mapSuccessState : (field1 -> field2) -> SuccessState field1 output -> SuccessState field2 output
mapSuccessState mapf successState =
    { successState | fields = successState.fields |> Map.mapBoth (\key successCell -> ( key |> mapf, mapSuccessCell mapf successCell )) }


merge : Map field (FailCell error field) -> Map field (FailCell error field) -> Map field (FailCell error field)
merge fields1 fields2 =
    fields1
        |> Map.foldl
            (\field failCell m ->
                m
                    |> Map.updateWithDefault field
                        (\mfailCell2 ->
                            case mfailCell2 of
                                Nothing ->
                                    failCell

                                Just failCell2 ->
                                    case failCell.error of
                                        Nothing ->
                                            failCell2

                                        _ ->
                                            failCell
                        )
            )
            fields2



-- setError : error -> FieldState error -> FieldState error
-- setError error fieldState =
--     { fieldState | error = Just error }
-- removeError : FieldState error -> FieldState error
-- removeError fieldState =
--     { fieldState | error = Nothing }
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
        |> Map.mapValue (\state -> { state | indexOfList = state.indexOfList |> Map.mapKey f })
