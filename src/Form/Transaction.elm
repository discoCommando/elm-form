module Form.Transaction exposing (..)

import Form.FieldIndex exposing (FieldIndex)
import Form.Map as Map
import Form.Types exposing (Form)
import Form.UniqueIndex exposing (UniqueIndex)
import Form.Validation
import Form.Values


type Transaction field
    = STR (Form.Types.Field String -> field) String
    | ADDROW field (UniqueIndex -> Transaction field)
    | REMOVEROW field UniqueIndex
    | BATCH (List (Transaction field))


batch : List (Transaction field) -> Transaction field
batch =
    BATCH


setString : (Form.Types.Field String -> field) -> String -> Transaction field
setString fieldF =
    STR fieldF


addRow : (Form.Types.FieldList (Form.Types.FieldNested x) -> field) -> Transaction x -> Transaction field
addRow fieldListF transaction =
    ADDROW (Form.Types.listOpaque fieldListF) (\uIdx -> transaction |> map (\x -> Form.Types.listField fieldListF uIdx (Form.Types.WithValue x)))


removeRow : (Form.Types.FieldList x -> field) -> UniqueIndex -> Transaction field
removeRow fieldListF =
    REMOVEROW (Form.Types.listOpaque fieldListF)


setNested : (Form.Types.FieldNested x -> field) -> Transaction x -> Transaction field
setNested fieldNF =
    map (\x -> Form.Types.fieldNestedNotOpaque fieldNF x)


map : (x -> field) -> Transaction x -> Transaction field
map mapF transaction =
    case transaction of
        STR f s ->
            STR (f >> mapF) s

        ADDROW f ut ->
            ADDROW (mapF f) (\uIdx -> ut uIdx |> map mapF)

        REMOVEROW f uIdx ->
            REMOVEROW (mapF f) uIdx

        BATCH ls ->
            BATCH (List.map (map mapF) ls)


save : Transaction field -> Form.Types.Form error field output -> Form.Types.Form error field output
save transaction form =
    form |> saveHelper transaction |> Tuple.second |> Form.Validation.validate


saveHelper : Transaction field -> Form.Types.Form error field output -> ( List FieldIndex, Form.Types.Form error field output )
saveHelper transaction form =
    case transaction of
        STR fieldF string ->
            let
                ( fieldIndex, newForm ) =
                    case form.fieldIndexes |> Map.get (fieldF Form.Types.Field) of
                        Nothing ->
                            ( form.fieldIndexToUse
                            , { form
                                | fieldIndexes = form.fieldIndexes |> Map.set (fieldF Form.Types.Field) form.fieldIndexToUse
                                , fieldIndexToUse = form.fieldIndexToUse |> Form.FieldIndex.next
                              }
                            )

                        Just fi ->
                            ( fi, form )

                newValues =
                    case newForm.values |> Form.Values.get fieldIndex of
                        Nothing ->
                            newForm.values |> Form.Values.set fieldIndex { value = Form.Types.stringValue string, error = Nothing }

                        Just state ->
                            newForm.values |> Form.Values.set fieldIndex { state | value = Form.Types.stringValue string }
            in
            ( [ fieldIndex ], { newForm | values = newValues } )

        ADDROW field transactionF ->
            let
                ( fieldIndex, newForm ) =
                    case form.fieldIndexes |> Map.get field of
                        Nothing ->
                            ( form.fieldIndexToUse
                            , { form
                                | fieldIndexes = form.fieldIndexes |> Map.set field form.fieldIndexToUse
                                , fieldIndexToUse = form.fieldIndexToUse |> Form.FieldIndex.next
                              }
                            )

                        Just fi ->
                            ( fi, form )

                ( fieldIndexes, newForm2 ) =
                    saveHelper (transactionF newForm.uniqueIndexToUse) { newForm | uniqueIndexToUse = newForm.uniqueIndexToUse |> Form.UniqueIndex.next }

                newListIndexes =
                    case newForm.listIndexes |> Form.Values.get fieldIndex of
                        Nothing ->
                            newForm.listIndexes
                                |> Form.Values.set fieldIndex
                                    (Map.empty
                                        |> Map.set newForm.uniqueIndexToUse
                                            (fieldIndexes |> List.foldl (\fi -> Form.Values.set fi ()) Form.Values.empty)
                                    )

                        Just uniqueIndexes ->
                            newForm.listIndexes
                                |> Form.Values.set fieldIndex
                                    (uniqueIndexes
                                        |> Map.set newForm.uniqueIndexToUse
                                            (fieldIndexes |> List.foldl (\fi -> Form.Values.set fi ()) Form.Values.empty)
                                    )
            in
            ( fieldIndex :: fieldIndexes, { newForm2 | listIndexes = newListIndexes } )

        REMOVEROW field uniqueIndex ->
            case form.fieldIndexes |> Map.get field of
                Just fieldIndex ->
                    case form.listIndexes |> Form.Values.get fieldIndex |> Maybe.andThen (Map.get uniqueIndex) of
                        Nothing ->
                            ( [], form )

                        Just fieldIndexSet ->
                            ( [], removeRowHelper (fieldIndexSet |> Form.Values.keys) form )

                Nothing ->
                    ( [], form )

        BATCH ls ->
            ls
                |> List.foldl
                    (\transaction ( result, newForm ) ->
                        let
                            ( newResult, newForm2 ) =
                                saveHelper transaction newForm
                        in
                        ( result ++ newResult, newForm2 )
                    )
                    ( [], form )


removeRowHelper : List FieldIndex -> Form error field output -> Form error field output
removeRowHelper states form =
    case states of
        [] ->
            form

        fieldIndex :: rest ->
            let
                newValues =
                    form.values |> Form.Values.remove fieldIndex

                newRest =
                    case form.listIndexes |> Form.Values.get fieldIndex of
                        Nothing ->
                            rest

                        Just m ->
                            rest ++ (m |> Map.toList |> List.map (Tuple.second >> Form.Values.keys) |> List.concat)
            in
            removeRowHelper newRest { form | values = newValues, listIndexes = form.listIndexes |> Form.Values.remove fieldIndex }
