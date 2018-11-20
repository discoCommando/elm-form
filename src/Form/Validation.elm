module Form.Validation exposing (..)

import Form.Fields
import Form.Map as Map
import Form.Types exposing (..)


fromString : (Field String -> field) -> (String -> Result error output) -> Validation error field output
fromString fieldF stringValidation fields =
    let
        fieldKey =
            Form.Types.field fieldF

        fieldState =
            case Map.get fieldKey fields of
                Just fs ->
                    fs

                Nothing ->
                    newFieldState (stringValue "")

        string =
            case asString fieldState.value of
                Just s ->
                    s

                _ ->
                    ""

        ( finalFieldState, output ) =
            case stringValidation string of
                Ok o ->
                    ( removeError fieldState, Just o )

                Err error ->
                    ( setError error fieldState, Nothing )
    in
    ( fields |> Form.Fields.setInValidation fieldKey finalFieldState, output )


fromNested : (x -> field) -> (field -> Maybe x) -> Validation error x output -> Validation error field output
fromNested lift filterFunction nestedValidation fields =
    let
        filteredFields =
            Map.filterMap filterFunction fields

        ( newFilteredFields, output ) =
            case nestedValidation filteredFields of
                ( newFilteredFields, output ) ->
                    ( newFilteredFields |> mapFields lift, output )
    in
    ( fields |> Form.Fields.mergeWithInValidation newFilteredFields, output )


fromNestedList : (Int -> x -> field) -> (field -> Maybe ( Int, x )) -> Validation error x output -> Validation error field (List output)
fromNestedList lift filterFunction nestedValidation fields =
    let
        filteredFieldsList =
            Map.filterMapList filterFunction fields

        ( newFilteredFields, output ) =
            case filteredFieldsList |> List.map nestedValidation |> List.unzip of
                ( newFields, outputList ) ->
                    ( newFields |> List.indexedMap (\i m -> m |> Map.mapKey (lift i)) |> List.foldl Map.mergeWith Map.empty
                    , let
                        filterMapped =
                            outputList |> List.filterMap identity
                      in
                      case List.length filterMapped == List.length outputList of
                        True ->
                            Just filterMapped

                        False ->
                            Nothing
                    )
    in
    ( fields |> Form.Fields.mergeWithInValidation newFilteredFields, output )


fromStringList : (Int -> Field String -> field) -> (String -> Result error output) -> Validation error field (List output)
fromStringList fieldF stringValidation fields =
    let
        fieldsLength =
            Map.length fields

        fieldsRange =
            List.range 0 (fieldsLength - 1)

        existingFields =
            fieldsRange
                |> List.filterMap
                    (\i ->
                        fields
                            |> Map.get (field (fieldF i))
                            |> Maybe.map
                                (\state ->
                                    ( field (fieldF i)
                                    , state
                                    )
                                )
                    )
                |> List.map (\( key, state ) -> ( key, state, state |> .value >> asString >> Maybe.withDefault "" >> stringValidation ))

        newFields =
            existingFields
                |> List.map
                    (\( key, state, res ) ->
                        case res of
                            Err x ->
                                ( key, setError x state )

                            Ok _ ->
                                ( key, removeError state )
                    )
                |> List.foldl (\( key, state ) -> Form.Fields.setInValidation key state) Map.empty

        output =
            existingFields
                |> List.filterMap (\( key, state, res ) -> Result.toMaybe res)
                |> (\filtered ->
                        case List.length filtered == List.length existingFields of
                            True ->
                                Just filtered

                            _ ->
                                Nothing
                   )
    in
    ( fields |> Form.Fields.mergeWithInValidation newFields, output )


string : String -> Result error String
string =
    Ok


int : String -> Result () Int
int =
    String.toInt >> Result.mapError (\_ -> ())


succeed : result -> Validation error field result
succeed result fields =
    ( fields, Just result )


validateForm : Validation error field output -> Form error field output -> Form error field output
validateForm validation form =
    let
        ( fields, output ) =
            validation form.fields
    in
    { form | fields = fields, output = output }


andMap : Validation field error a -> Validation field error (a -> b) -> Validation field error b
andMap validation1 validation2 fields =
    let
        ( fields1, output1 ) =
            validation1 fields

        ( fields2, output2 ) =
            validation2 fields1
    in
    ( fields2
    , output1
        |> Maybe.andThen
            (\a ->
                output2
                    |> Maybe.map (\f -> f a)
            )
    )
