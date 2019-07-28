module Form.Transaction exposing (addRow, batch, empty, map, removeRow, removeRowHelper, save, saveHelper, setAtIndex, setNested, setString)

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


setAtIndex : (Form.FieldList x -> field) -> UniqueIndex -> Transaction x -> Transaction field
setAtIndex fieldListF uniqueIndex transaction =
    T_SETINLIST (Form.listOpaque fieldListF) uniqueIndex (transaction |> map (\x -> Form.listField fieldListF uniqueIndex x))


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

        T_SETINLIST listFieldOpaque uniqueIndex transaction ->
            T_SETINLIST (mapF listFieldOpaque) uniqueIndex (map mapF transaction)


save : Transaction field -> Form error field output -> Form error field output
save transaction form =
    form |> saveHelper transaction |> Tuple.first |> Form.validate


empty : Transaction field
empty =
    batch []


saveHelper : Transaction field -> Form error field output -> ( Form error field output, List FieldIndex )
saveHelper transaction form =
    case transaction of
        T_STR fieldF string ->
            let
                ( newForm, fieldIndex ) =
                    Form.getFieldIndex (Form.field fieldF) form

                newValues =
                    case newForm.values |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            newForm.values |> FieldIndexDict.set fieldIndex { value = Form.FieldState.stringValue string, error = Nothing }

                        Just state ->
                            newForm.values |> FieldIndexDict.set fieldIndex { state | value = Form.FieldState.stringValue string }
            in
            ( { newForm | values = newValues }, [ fieldIndex ] )

        T_ADDROW field transactionF ->
            let
                ( newForm, fieldIndex ) =
                    Form.getFieldIndex field form

                uniqueIndexToSave =
                    newForm.uniqueIndexToUse

                ( newForm2, fieldIndexes ) =
                    saveHelper (transactionF uniqueIndexToSave) { newForm | uniqueIndexToUse = newForm.uniqueIndexToUse |> Index.UniqueIndex.next }

                newListIndexes =
                    newForm2.listIndexes
                        |> FieldIndexDict.update fieldIndex
                            (\muniqueIndexes ->
                                muniqueIndexes
                                    |> Maybe.withDefault UniqueIndexDict.empty
                                    |> UniqueIndexDict.set uniqueIndexToSave
                                        { fieldIndexSet = fieldIndexes |> List.foldl (\fi -> FieldIndexDict.set fi ()) FieldIndexDict.empty
                                        , order = newForm2.counter
                                        }
                                    |> Just
                            )
            in
            ( { newForm2 | listIndexes = newListIndexes, counter = newForm2.counter + 1 }, fieldIndex :: fieldIndexes )

        T_SETINLIST listFieldOpaque uniqueIndex transaction ->
            let
                ( newForm, fieldIndex ) =
                    Form.getFieldIndex listFieldOpaque form
            in
            case newForm.listIndexes |> FieldIndexDict.get fieldIndex of
                Nothing ->
                    ( newForm, [] )

                Just uniqueIndexes ->
                    case uniqueIndexes |> UniqueIndexDict.get uniqueIndex of
                        Nothing ->
                            ( form, [] )

                        Just fieldIndexSetState ->
                            let
                                ( newForm2, fieldIndexes ) =
                                    saveHelper transaction newForm
                            in
                            ( { newForm2
                                | listIndexes =
                                    newForm2.listIndexes
                                        |> FieldIndexDict.set fieldIndex
                                            (uniqueIndexes
                                                |> UniqueIndexDict.set uniqueIndex
                                                    { fieldIndexSetState
                                                        | fieldIndexSet = fieldIndexes |> List.foldl (\fi -> FieldIndexDict.set fi ()) fieldIndexSetState.fieldIndexSet
                                                    }
                                            )
                              }
                            , fieldIndex :: fieldIndexes
                            )

        T_REMOVEROW field uniqueIndex ->
            case form.fieldIndexes |> Map.get field of
                Just fieldIndex ->
                    case form.listIndexes |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            ( form, [] )

                        Just uniqueIndexes ->
                            case uniqueIndexes |> UniqueIndexDict.get uniqueIndex of
                                Nothing ->
                                    ( form, [] )

                                Just fieldIndexSetState ->
                                    ( { form
                                        | listIndexes =
                                            form.listIndexes
                                                |> FieldIndexDict.set fieldIndex (uniqueIndexes |> UniqueIndexDict.remove uniqueIndex)
                                      }
                                        |> removeRowHelper (fieldIndexSetState.fieldIndexSet |> FieldIndexDict.keys)
                                    , []
                                    )

                Nothing ->
                    ( form, [] )

        T_BATCH ls ->
            ls
                |> List.foldl
                    (\transaction ( newForm, result ) ->
                        let
                            ( newForm2, newResult ) =
                                saveHelper transaction newForm
                        in
                        ( newForm2, result ++ newResult )
                    )
                    ( form, [] )


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
                            rest ++ (m |> UniqueIndexDict.values |> List.map (.fieldIndexSet >> FieldIndexDict.keys) |> List.concat)
            in
            removeRowHelper newRest { form | values = newValues, listIndexes = form.listIndexes |> FieldIndexDict.remove fieldIndex }
