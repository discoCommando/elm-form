module Form.Transaction exposing
    ( Transaction
    , addRow
    , batch
    , empty
    , map
    , removeRow
    , removeRowHelper
    , save
    , saveHelper
    , setAtIndex
    , setNested
    , setString
    , setBool
    )

import Form.Field as Field
import Form.FieldState
import Form.Map as Map
import Form.Validation exposing (Validation)
import Index.FieldIndex as FieldIndex exposing (FieldIndex)
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex exposing (UniqueIndex)
import Index.UniqueIndexDict as UniqueIndexDict


type Transaction field
    = T_STR (Field.Value String -> field) String
    | T_BOOL (Field.Value Bool -> field) Bool 
    | T_ADDROW field (UniqueIndex -> Transaction field)
    | T_SETINLIST field UniqueIndex (Transaction field)
    | T_REMOVEROW field UniqueIndex
    | T_BATCH (List (Transaction field))


type alias Form error field output =
    Form.Validation.Form error field output


batch : List (Transaction field) -> Transaction field
batch =
    T_BATCH


setString : (Field.Value String -> field) -> String -> Transaction field
setString =
    T_STR

setBool : (Field.Value Bool -> field) -> Bool -> Transaction field 
setBool = 
    T_BOOL


addRow : (Field.List x -> field) -> Transaction x -> Transaction field
addRow fieldListF transaction =
    T_ADDROW (fieldListF Field.OpaqueList) (\uIdx -> transaction |> map (\x -> fieldListF <| Field.WithIndex uIdx x))


removeRow : (Field.List x -> field) -> UniqueIndex -> Transaction field
removeRow fieldListF =
    T_REMOVEROW (fieldListF Field.OpaqueList)


setAtIndex : (Field.List x -> field) -> UniqueIndex -> Transaction x -> Transaction field
setAtIndex fieldListF uniqueIndex transaction =
    T_SETINLIST (fieldListF Field.OpaqueList) uniqueIndex (transaction |> map (\x -> fieldListF <| Field.WithIndex uniqueIndex x))


setNested : (Field.Nested x -> field) -> Transaction x -> Transaction field
setNested fieldNF =
    map (\x -> fieldNF <| Field.WithValue x)


map : (x -> field) -> Transaction x -> Transaction field
map mapF transaction =
    case transaction of
        T_STR f s ->
            T_STR (f >> mapF) s

        T_BOOL f b ->
            T_BOOL (f >> mapF) b

        T_ADDROW f ut ->
            T_ADDROW (mapF f) (\uIdx -> ut uIdx |> map mapF)

        T_REMOVEROW f uIdx ->
            T_REMOVEROW (mapF f) uIdx

        T_BATCH ls ->
            T_BATCH (List.map (map mapF) ls)

        T_SETINLIST listFieldOpaque uniqueIndex transaction_ ->
            T_SETINLIST (mapF listFieldOpaque) uniqueIndex (map mapF transaction_)


save : Transaction field -> Form error field output -> Form error field output
save transaction form =
    form |> saveHelper transaction |> Tuple.first |> Form.Validation.validate_


empty : Transaction field
empty =
    batch []


saveHelper : Transaction field -> Form error field output -> ( Form error field output, List FieldIndex )
saveHelper transaction form =
    case transaction of
        T_STR fieldF string ->
            let
                ( newForm, fieldIndex ) =
                    getFieldIndex (fieldF Field.Value) form

                newValues =
                    case newForm.values |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            newForm.values |> FieldIndexDict.set fieldIndex { value = Form.FieldState.stringValue string, error = Nothing }

                        Just state ->
                            newForm.values |> FieldIndexDict.set fieldIndex { state | value = Form.FieldState.stringValue string }
            in
            ( { newForm | values = newValues }, [ fieldIndex ] )

        T_BOOL fieldF bool ->
            let
                ( newForm, fieldIndex ) =
                    getFieldIndex (fieldF Field.Value) form

                newValues =
                    case newForm.values |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            newForm.values |> FieldIndexDict.set fieldIndex { value = Form.FieldState.boolValue bool, error = Nothing }

                        Just state ->
                            newForm.values |> FieldIndexDict.set fieldIndex { state | value = Form.FieldState.boolValue bool }
            in
            ( { newForm | values = newValues }, [ fieldIndex ] )

        T_ADDROW field transactionF ->
            let
                ( newForm, fieldIndex ) =
                    getFieldIndex field form

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

        T_SETINLIST listFieldOpaque uniqueIndex transaction_ ->
            let
                ( newForm, fieldIndex ) =
                    getFieldIndex listFieldOpaque form
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
                    (\transaction_ ( newForm, result ) ->
                        let
                            ( newForm2, newResult ) =
                                saveHelper transaction_ newForm
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


getFieldIndex : field -> Form error field output -> ( Form error field output, FieldIndex )
getFieldIndex field_ form_ =
    case form_.fieldIndexes |> Map.get field_ of
        Nothing ->
            ( { form_
                | fieldIndexes = form_.fieldIndexes |> Map.set field_ form_.fieldIndexToUse
                , fieldIndexToUse = form_.fieldIndexToUse |> FieldIndex.next
              }
            , form_.fieldIndexToUse
            )

        Just fi ->
            ( form_, fi )
