module Form.Fields exposing (..)

import Form.Map as Map
import Form.Types exposing (..)
import Form.Validation


get : Form error field output -> Fields error field
get form =
    form.fields


transaction : Form error field output -> Transaction error field output
transaction form =
    { newFields = Map.empty
    , initialForm = form
    }


newFieldState : FieldValue -> FieldState error
newFieldState fv =
    { value = fv, error = Nothing }


setFieldString : (Field String -> field) -> String -> Transaction error field output -> Transaction error field output
setFieldString fieldF str transaction =
    { transaction | newFields = transaction.newFields |> Map.set (field fieldF) (newFieldState (stringValue str)) }



-- setFieldNested : (nested -> field) -> (Transaction error nested output -> )


save : Transaction error field output -> Form error field output
save transaction =
    let
        form =
            transaction.initialForm
    in
    form.fields
        |> Map.mergeWith transaction.newFields
        |> Form.Validation.validate form.validation
        |> (\( newFields, output ) -> { form | fields = newFields, output = output })


setInValidation : field -> FieldState error -> Fields error field -> Fields error field
setInValidation field fieldState fields =
    let
        fieldToSet =
            case fields |> Map.get field of
                Nothing ->
                    fieldState

                Just fieldStateInitial ->
                    case fieldState.error of
                        Nothing ->
                            fieldStateInitial

                        Just error ->
                            fieldState
    in
    fields |> Map.set field fieldToSet


mergeWithInValidation : Fields error field -> Fields error field -> Fields error field
mergeWithInValidation fields2 fields1 =
    fields1 |> Map.toList |> List.foldl (\( k, v ) map -> setInValidation k v map) fields2


getFieldsList : (Int -> Field x -> field) -> Fields error field -> List ( field, FieldState error )
getFieldsList fieldF fields =
    let
        key i =
            fieldF i Form.Types.Field

        length =
            Map.length fields
    in
    List.range 0 (length - 1) |> List.filterMap (\i -> fields |> Map.get (key i) |> Maybe.map (\x -> ( key i, x )))
