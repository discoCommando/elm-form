module Form.Get exposing (ValueState(..), atList, field, getBool, getError, getHelper, getString, indexes, nested, toMaybe)

import Form.Field as Field
import Form.FieldState as FieldState exposing (InternalFieldState)
import Form.Map as Map
import Form.Type exposing (Form)
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex exposing (UniqueIndex)
import Index.UniqueIndexDict as UniqueIndexDict


type Get field resultType
    = Get (Field.Value resultType -> field)

type ValueState resultType 
    = Edited resultType 
    | NotEdited 

toMaybe : ValueState a -> Maybe a
toMaybe r =
    case r of
        Edited a ->
            Just a

        NotEdited ->
            Nothing

fromMaybe : Maybe a -> ValueState a 
fromMaybe ma = 
    case ma of 
        Just a -> 
            Edited a 

        Nothing -> 
            NotEdited 


field : (Field.Value a -> field) -> Get field a
field =
    Get


getString : Get field String -> Form error field output validation submitted -> ValueState String
getString =
    getHelper
        (Maybe.andThen FieldState.getFieldValue 
            >> Maybe.andThen FieldState.getString 
            >> fromMaybe) 


getBool : Get field Bool -> Form error field output validation submitted -> ValueState Bool
getBool =
    getHelper
        (Maybe.andThen FieldState.getFieldValue 
            >> Maybe.andThen FieldState.getBool
            >> fromMaybe) 

getError : Get field a -> Form error field output validation submitted -> FieldState.ErrorState error 
getError =
    getHelper
        (Maybe.map .errorState >> Maybe.withDefault FieldState.NoError)



getHelper : (Maybe (InternalFieldState error) -> x) -> Get field a -> Form error field output validation submitted -> x
getHelper f (Get fieldF) form_ =
    case form_.fieldIndexes |> Map.get (fieldF Field.Value) of
        Nothing ->
            f Nothing

        Just fieldIndex ->
            form_.values |> FieldIndexDict.get fieldIndex |> f


nested : (Field.Nested nested -> field) -> Get nested a -> Get field a
nested fieldNested (Get nestedF) =
    Get (\field_ -> fieldNested <| Field.WithValue <| nestedF field_)


atList : (Field.List nested -> field) -> UniqueIndex -> Get nested a -> Get field a
atList fieldList uniqueIndex (Get nestedF) =
    Get (\field_ -> fieldList <| Field.WithIndex uniqueIndex <| nestedF field_)


indexes : (Field.List x -> field) -> Form error field output validation submitted -> List UniqueIndex
indexes fieldF form_ =
    case form_.fieldIndexes |> Map.get (fieldF Field.OpaqueList) of
        Nothing ->
            []

        Just fieldIndex ->
            case form_.listIndexes |> FieldIndexDict.get fieldIndex of
                Nothing ->
                    []

                Just uniqueIndexes ->
                    uniqueIndexes |> UniqueIndexDict.toList |> List.sortBy (Tuple.second >> .order) |> List.map Tuple.first
