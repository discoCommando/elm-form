module Form.Fields exposing (..)

import Form.Map as Map
import Form.Types exposing (..)


-- import Form.Validation
-- get : Form error field output -> Fields error field
-- get form =
--     form.fields
--
-- transaction : Form error field output -> Transaction error field output
-- transaction form =
--     { newFields = Map.empty
--     , initialForm = form
--     }


newFieldState : FieldValue -> FieldState error
newFieldState fv =
    { value = fv, error = Nothing }



-- setFieldString : (Field String -> field) -> String -> Transaction error field output -> Transaction error field output
-- setFieldString fieldF str transaction =
--     { transaction | newFields = transaction.newFields |> Map.set (field fieldF) (newFieldState (stringValue str)) }
-- addToList : (FieldList a -> field) -> Transaction error field output -> Transaction error field output
-- addToList fieldListF transaction =
--     { transaction | newFields = transaction.newFields |> Form.Types.addIndex fieldListF }
-- setFieldStringInList : (FieldList )
-- setFieldNested : (nested -> field) -> (Transaction error nested output -> )
-- save : Transaction error field output -> Form error field output
-- save transaction =
--     let
--         form =
--             transaction.initialForm
--     in
--     form.fields
--         |> Map.mergeWith transaction.newFields
--         |> Form.Validation.validate form.validation
--         |> (\( newFields, output ) -> { form | fields = newFields, output = output })
-- getFieldsList : (Int -> Field x -> field) -> Fields error field -> List ( field, FieldState error )
-- getFieldsList fieldF fields =
--     let
--         key i =
--             fieldF i Form.Types.Field
--         length =
--             Map.length fields
--     in
--     List.range 0 (length - 1) |> List.filterMap (\i -> fields |> Map.get (key i) |> Maybe.map (\x -> ( key i, x )))
-- set : field -> FieldValue -> Fields error field -> Fields error field
-- set field fieldValue fields =
--     let
--         oldState =
--             fields |> Map.get field |> Maybe.withDefault (newFieldState fieldValue)
--     in
--     fields |> Map.set field { oldState | value = fieldValue }
-- deleteAllWithIndex : field -> Int -> Fields error field -> Fields error field
-- deleteAllWithIndex field i1 fields =
--     fields
--         |> Map.filter
--             (\key state ->
--                 case Map.get field state.indexOfList of
--                     Just i2 ->
--                         i1 /= i2
--
--                     Nothing ->
--                         True
--             )
--
--
-- addIndexOfList : field -> field -> Int -> Fields error field -> Fields error field
-- addIndexOfList key index i fields =
--     fields
--         |> Map.update key (\state -> { state | indexOfList = state.indexOfList |> Map.set index i })
--
-- addToRowLength : field -> Fields error field -> Fields error field
-- addToRowLength key fields =
--     case Map.get key fields of
--         Nothing ->
--             fields |> Map.set key (newFieldState (lengthValue 1))
--
--         Just state ->
--             fields |> Map.set key { state | value = state.value |> asLength |> Maybe.withDefault 0 |> (+) 1 |> lengthValue }
-- removeFromRowLength : field -> Fields error field -> Fields error field
-- removeFromRowLength key fields =
--     case Map.get key fields of
--         Nothing ->
--             fields
--
--         Just state ->
--             fields
--                 |> Map.set key
--                     { state
--                         | value =
--                             state.value
--                                 |> asLength
--                                 |> Maybe.withDefault 0
--                                 |> (\x ->
--                                         case x of
--                                             0 ->
--                                                 0
--
--                                             _ ->
--                                                 x - 1
--                                    )
--                                 |> lengthValue
--                     }
