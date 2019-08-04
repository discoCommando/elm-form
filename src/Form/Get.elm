module Form.Get exposing (atList, field, getError, getHelper, getString, nested)

import Form exposing (Field, FieldList(..), FieldNested(..), Form, Get(..))
import Form.FieldState exposing (FieldState)
import Form.Map as Map
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndexDict as UniqueIndexDict
import Index.UniqueIndex exposing (UniqueIndex)


field : (Field a -> field) -> Get field a
field =
    Get


getString : Get field String -> Form error field output -> String
getString =
    getHelper <|
        \mFieldState ->
            case mFieldState of
                Nothing ->
                    ""

                Just fieldState ->
                    case fieldState.value of
                        Form.FieldState.FVString s ->
                            s

                        _ ->
                            ""


getError : Get field a -> Form error field output -> Maybe error
getError =
    getHelper <|
        \mFieldState ->
            mFieldState |> Maybe.andThen .error


getHelper : (Maybe (FieldState error) -> x) -> Get field a -> Form error field output -> x
getHelper f (Get fieldF) form_ =
    case form_.fieldIndexes |> Map.get (fieldF Form.Field) of
        Nothing ->
            f Nothing

        Just fieldIndex ->
            form_.values |> FieldIndexDict.get fieldIndex |> f


nested : (FieldNested nested -> field) -> Get nested a -> Get field a
nested fieldNested (Get nestedF) =
    Get (\field_ -> fieldNested <| Form.WithValue <| nestedF field_)


atList : (FieldList nested -> field) -> UniqueIndex -> Get nested a -> Get field a
atList fieldList uniqueIndex (Get nestedF) =
    Get (\field_ -> fieldList <| Form.WithIndex uniqueIndex <| nestedF field_)


indexes : (FieldList x -> field) -> Form error field output -> List UniqueIndex
indexes fieldF form_ =
    case form_.fieldIndexes |> Map.get (listOpaque fieldF) of
        Nothing ->
            []

        Just fieldIndex ->
            case form_.listIndexes |> FieldIndexDict.get fieldIndex of
                Nothing ->
                    []

                Just uniqueIndexes ->
                    uniqueIndexes |> UniqueIndexDict.toList |> List.sortBy (Tuple.second >> .order) |> List.map Tuple.first