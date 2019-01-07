module Form.Transaction exposing (..)

import Form exposing (Form, Transaction(..))
import Form.FieldState
import Form.Map as Map
import Index.FieldIndex exposing (FieldIndex)
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex exposing (UniqueIndex)
import Index.UniqueIndexDict as UniqueIndexDict


batch : List (Transaction field) -> Transaction field
batch =
    T_BATCH


setString : (Form.Field String -> field) -> String -> Transaction field
setString fieldF =
    T_STR fieldF


addRow : (Form.FieldList x -> field) -> Transaction x -> Transaction field
addRow fieldListF transaction =
    T_ADDROW (Form.listOpaque fieldListF) (\uIdx -> transaction |> map (\x -> Form.listField fieldListF uIdx x))


removeRow : (Form.FieldList x -> field) -> UniqueIndex -> Transaction field
removeRow fieldListF =
    T_REMOVEROW (Form.listOpaque fieldListF)


setNested : (Form.FieldNested x -> field) -> Transaction x -> Transaction field
setNested fieldNF =
    map (\x -> Form.fieldNestedNotOpaque fieldNF x)


map : (x -> field) -> Transaction x -> Transaction field
map mapF transaction =
    case transaction of
        T_STR f s ->
            T_STR (f >> mapF) s

        T_ADDROW f ut ->
            T_ADDROW (mapF f) (\uIdx -> ut uIdx |> map mapF)

        T_REMOVEROW f uIdx ->
            T_REMOVEROW (mapF f) uIdx

        T_BATCH ls ->
            T_BATCH (List.map (map mapF) ls)


save : Transaction field -> Form error field output -> Form error field output
save transaction form =
    form |> saveHelper transaction |> Tuple.second |> Form.validate


saveHelper : Transaction field -> Form error field output -> ( List FieldIndex, Form error field output )
saveHelper transaction form =
    case transaction of
        T_STR fieldF string ->
            let
                ( fieldIndex, newForm ) =
                    case form.fieldIndexes |> Map.get (fieldF Form.Field) of
                        Nothing ->
                            ( form.fieldIndexToUse
                            , { form
                                | fieldIndexes = form.fieldIndexes |> Map.set (fieldF Form.Field) form.fieldIndexToUse
                                , fieldIndexToUse = form.fieldIndexToUse |> Index.FieldIndex.next
                              }
                            )

                        Just fi ->
                            ( fi, form )

                newValues =
                    case newForm.values |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            newForm.values |> FieldIndexDict.set fieldIndex { value = Form.FieldState.stringValue string, error = Nothing }

                        Just state ->
                            newForm.values |> FieldIndexDict.set fieldIndex { state | value = Form.FieldState.stringValue string }
            in
            ( [ fieldIndex ], { newForm | values = newValues } )

        T_ADDROW field transactionF ->
            let
                ( fieldIndex, newForm ) =
                    case form.fieldIndexes |> Map.get field of
                        Nothing ->
                            ( form.fieldIndexToUse
                            , { form
                                | fieldIndexes = form.fieldIndexes |> Map.set field form.fieldIndexToUse
                                , fieldIndexToUse = form.fieldIndexToUse |> Index.FieldIndex.next
                              }
                            )

                        Just fi ->
                            ( fi, form )

                ( fieldIndexes, newForm2 ) =
                    saveHelper (transactionF newForm.uniqueIndexToUse) { newForm | uniqueIndexToUse = newForm.uniqueIndexToUse |> Index.UniqueIndex.next }

                newListIndexes =
                    case newForm.listIndexes |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            newForm2.listIndexes
                                |> FieldIndexDict.set fieldIndex
                                    (UniqueIndexDict.empty
                                        |> UniqueIndexDict.set newForm.uniqueIndexToUse
                                            (fieldIndexes |> List.foldl (\fi -> FieldIndexDict.set fi ()) FieldIndexDict.empty)
                                    )

                        Just uniqueIndexes ->
                            newForm2.listIndexes
                                |> FieldIndexDict.set fieldIndex
                                    (uniqueIndexes
                                        |> UniqueIndexDict.set newForm.uniqueIndexToUse
                                            (fieldIndexes |> List.foldl (\fi -> FieldIndexDict.set fi ()) FieldIndexDict.empty)
                                    )
            in
            ( fieldIndex :: fieldIndexes, { newForm2 | listIndexes = newListIndexes } )

        T_REMOVEROW field uniqueIndex ->
            case form.fieldIndexes |> Map.get field of
                Just fieldIndex ->
                    case form.listIndexes |> FieldIndexDict.get fieldIndex |> Maybe.andThen (UniqueIndexDict.get uniqueIndex) of
                        Nothing ->
                            ( [], form )

                        Just fieldIndexSet ->
                            ( [], removeRowHelper (fieldIndexSet |> FieldIndexDict.keys) form )

                Nothing ->
                    ( [], form )

        T_BATCH ls ->
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
                    form.values |> FieldIndexDict.remove fieldIndex

                newRest =
                    case form.listIndexes |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            rest

                        Just m ->
                            rest ++ (m |> UniqueIndexDict.values |> List.map FieldIndexDict.keys |> List.concat)
            in
            removeRowHelper newRest { form | values = newValues, listIndexes = form.listIndexes |> FieldIndexDict.remove fieldIndex }
