module Form.Transaction exposing (..)

import Form.Fields
import Form.Map as Map
import Form.Types
import Form.Validation


type Transaction field
    = STR field (List ( field, Int )) String
    | ADDROW field
    | REMOVEROW field
    | BATCH (List (Transaction field))
    | LIST field Int (Transaction field)


batch : List (Transaction field) -> Transaction field
batch =
    BATCH


setString : (Form.Types.Field String -> field) -> String -> Transaction field
setString fieldF =
    STR (Form.Types.field fieldF) []


addRow : (Form.Types.FieldList x -> field) -> Transaction field
addRow fieldListF =
    ADDROW (Form.Types.listOpaque fieldListF)


removeRow : (Form.Types.FieldList x -> field) -> Transaction field
removeRow fieldListF =
    REMOVEROW (Form.Types.listOpaque fieldListF)


map : (x -> field) -> Transaction x -> Transaction field
map mapF transaction =
    case transaction of
        STR f iol x ->
            STR (mapF f) (iol |> List.map (Tuple.mapFirst mapF)) x

        ADDROW f ->
            ADDROW (mapF f)

        REMOVEROW f ->
            REMOVEROW (mapF f)

        BATCH ls ->
            BATCH (List.map (map mapF) ls)

        LIST field i transaction ->
            LIST (mapF field) i (transaction |> map mapF)


addIndexOfList : field -> Int -> Transaction field -> Transaction field
addIndexOfList key i transaction =
    case transaction of
        STR f iol x ->
            STR f (( key, i ) :: iol) x

        BATCH ls ->
            BATCH (ls |> List.map (addIndexOfList key i))

        LIST f i transaction2 ->
            LIST f i (addIndexOfList key i transaction2)

        _ ->
            transaction


setInList : (Form.Types.FieldList x -> field) -> Int -> Transaction x -> Transaction field
setInList fieldListf i transaction =
    LIST (Form.Types.listOpaque fieldListf) i (transaction |> map (Form.Types.listField fieldListf i))


save : Transaction field -> Form.Types.Form error field output -> Form.Types.Form error field output
save transaction form =
    let
        ( newFields, output ) =
            form.fields |> saveHelper transaction |> Form.Validation.validate form.validation
    in
    { form | fields = newFields, output = output }


saveHelper : Transaction field -> Form.Types.Fields error field -> Form.Types.Fields error field
saveHelper transaction fields =
    case transaction of
        STR field indexOfList string ->
            fields |> Map.set field { error = Nothing, value = Form.Types.stringValue string, indexOfList = indexOfList |> List.foldl (\( fld, i ) m -> m |> Map.set fld i) Map.empty }

        ADDROW field ->
            fields |> Form.Fields.addToRowLength field

        REMOVEROW field ->
            case Map.get field fields |> Maybe.andThen (.value >> Form.Types.asLength) of
                Nothing ->
                    fields

                Just x ->
                    case x <= 0 of
                        True ->
                            fields

                        False ->
                            fields |> Form.Fields.removeFromRowLength field |> Form.Fields.deleteAllWithIndex field (x - 1)

        BATCH ls ->
            ls |> List.foldl saveHelper fields

        LIST field i transaction ->
            transaction |> addIndexOfList field i |> (\t -> saveHelper t fields)
