module Form.Types exposing (..)

import Form.FieldIndex exposing (FieldIndex, Values)
import Form.Map as Map exposing (Map)
import Form.UniqueIndex exposing (UniqueIndex)


type Field a
    = Field


type FieldList a
    = OpaqueList
    | WithIndex UniqueIndex a


type FieldNested a
    = OpaqueNested
    | WithValue a


type alias Form error field output =
    { fieldIndexes : Map field FieldIndex
    , listIndexes : Values (Map UniqueIndex (Values ()))
    , values : Values (FieldState error)
    , submitted : Bool
    , validation : Validation error field output
    , output : Maybe output
    , fieldIndexToUse : FieldIndex
    , uniqueIndexToUse : UniqueIndex
    }


type alias Fields error field =
    Map field (FieldState error)



-- type alias ListsFields error field =
--     Map field (ListFieldState error)


type alias Transaction error field output =
    { newFields : Fields error field
    , initialForm : Form error field output
    }


type FieldValue
    = FVString String
    | FVBool Bool
    | FVEmpty -- should happen only once there is an error in validation


type alias FieldState error =
    { error : Maybe error
    , value : FieldValue
    }


type alias IndexOfList field =
    { getter : field
    , index : Int
    }


type alias FailCell error =
    { error : Maybe error
    }


type alias FailState error field =
    { errors : Map field (FailCell error) }


type alias SuccessCell field =
    { indexOfList : Map field Int
    }


type alias SuccessState output =
    { output : output
    }


type Validation error field output
    = STR (Field String -> field) (String -> Validation error field output)
    | LIST field (List UniqueIndex -> Validation error field output)
    | FAIL (FailState error field)
    | SUCCESS (SuccessState output)


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


listOpaque : (FieldList a -> field) -> field
listOpaque listF =
    listF OpaqueList


listField : (FieldList a -> field) -> UniqueIndex -> a -> field
listField listF i a =
    listF (WithIndex i a)


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
    fieldF OpaqueNested


fieldNestedNotOpaque : (FieldNested a -> field) -> a -> field
fieldNestedNotOpaque fieldF a =
    fieldF (WithValue a)


mapFailState : (field1 -> field2) -> FailState error field1 -> FailState error field2
mapFailState mapf failState =
    { failState | errors = failState.errors |> Map.mapBoth (\key failCell -> ( key |> mapf, failCell )) }


mapSuccessCell : (field1 -> field2) -> SuccessCell field1 -> SuccessCell field2
mapSuccessCell mapf successCell =
    { successCell | indexOfList = successCell.indexOfList |> Map.mapKey mapf }


merge : Map field (FailCell error) -> Map field (FailCell error) -> Map field (FailCell error)
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


mapFields : (a -> b) -> Fields error a -> Fields error b
mapFields f fields =
    fields
        |> Map.mapKey f
