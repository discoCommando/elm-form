module Form.Get exposing (Result(..), atList, field, getBool, getError, getHelper, getString, indexes, nested, toMaybe)

import Form.Field as Field
import Form.FieldState exposing (FieldState)
import Form.Map as Map
import Form.Type exposing (Form)
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex exposing (UniqueIndex)
import Index.UniqueIndexDict as UniqueIndexDict


type Get field resultType
    = Get (Field.Value resultType -> field)


type Result resultType
    = Edited resultType
    | NotEdited


toMaybe : Result a -> Maybe a
toMaybe r =
    case r of
        Edited a ->
            Just a

        NotEdited ->
            Nothing


field : (Field.Value a -> field) -> Get field a
field =
    Get


getString : Get field String -> Form error field output validation -> Result String
getString =
    getHelper <|
        \mFieldState ->
            case mFieldState of
                Nothing ->
                    NotEdited

                Just fieldState ->
                    Edited <|
                        case fieldState.value of
                            Form.FieldState.FVString s ->
                                s

                            _ ->
                                ""


getBool : Get field Bool -> Form error field output validation -> Result Bool
getBool =
    getHelper <|
        \mFieldState ->
            case mFieldState of
                Nothing ->
                    NotEdited

                Just fieldState ->
                    Edited <|
                        case fieldState.value of
                            Form.FieldState.FVBool b ->
                                b

                            _ ->
                                False


getError : Get field a -> Form error field output validation -> Maybe error
getError =
    getHelper <|
        \mFieldState ->
            mFieldState |> Maybe.andThen .error


getHelper : (Maybe (FieldState error) -> x) -> Get field a -> Form error field output validation -> x
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


indexes : (Field.List x -> field) -> Form error field output validation -> List UniqueIndex
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
