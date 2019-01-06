module Form.Validation exposing (..)

import Form.FieldIndex
import Form.Map as Map
import Form.Types
    exposing
        ( FailCell
        , FailState
        , Field
        , FieldList
        , FieldNested(..)
        , FieldValue(..)
        , Fields
        , Form
        , SuccessCell
        , SuccessState
        , Validation(..)
        , mapFailState
        )
import Form.UniqueIndex exposing (UniqueIndex)
import Form.Validation.Types exposing (..)


string : String -> Result () String
string s =
    case s of
        "" ->
            Err ()

        ss ->
            Ok ss


int : String -> Result () Int
int =
    String.toInt >> Result.mapError (\_ -> ())


optional : Result error output -> Result error (Maybe output)
optional result =
    case result of
        Ok v ->
            Ok (Just v)

        _ ->
            Ok Nothing


fromString : (Field String -> field) -> (String -> Result error output) -> Validation error field output
fromString fieldF strVal =
    STR
        fieldF
        (\s ->
            case strVal s of
                Err error ->
                    FAIL <| FailState <| Map.singleton (Form.Types.field fieldF) <| FailCell (Just error)

                Ok v ->
                    SUCCESS <| SuccessState v
        )


mapField : (field1 -> field2) -> Validation error field1 output -> Validation error field2 output
mapField mapF validation =
    case validation of
        -- PURE output ->
        --     PURE output
        -- ERROR errors ->
        --     ERROR (errors |> List.map (\(field, error) -> (mapF field, error)))
        STR f1 cont ->
            STR (f1 >> mapF) (\s -> cont s |> mapField mapF)

        -- LIST f1i cont ->
        --     LIST (\i f -> f1i i f |> mapF) (\ls -> cont ls |> mapField mapF)
        LIST fl fi ->
            LIST (fl |> mapF) (\uidxs -> fi uidxs |> mapField mapF)

        -- INPROGRESS errors rest ->
        --     INPROGRESS (errors |> List.map (Tuple.mapFirst mapF)) (rest |> mapField mapF)
        FAIL failState ->
            FAIL <| mapFailState mapF failState

        SUCCESS successState ->
            SUCCESS successState


fromNested : (FieldNested x -> field) -> Validation error x output -> Validation error field output
fromNested fieldNF validation =
    mapField (\x -> Form.Types.fieldNestedNotOpaque fieldNF x) validation


fromListString : (FieldList (Field String) -> field) -> (String -> Result error output) -> Validation error field (List output)
fromListString fieldListF strValidation =
    -- fromList fieldListF (fromString identity strValidation)
    LIST (Form.Types.listOpaque fieldListF)
        (makeList fieldListF (fromString identity strValidation))


fromList : (FieldList (FieldNested x) -> field) -> Validation error x output -> Validation error field (List output)
fromList fieldListF validation =
    LIST (Form.Types.listOpaque fieldListF)
        (makeList fieldListF (validation |> mapField Form.Types.WithValue))


map : (o1 -> o2) -> Validation error field o1 -> Validation error field o2
map f validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> map f)

        LIST fl fi ->
            LIST fl (\uidxs -> fi uidxs |> map f)

        FAIL failState ->
            FAIL failState

        SUCCESS successState ->
            SUCCESS { successState | output = successState.output |> f }


andThen : (output1 -> Validation error field output2) -> Validation error field output1 -> Validation error field output2
andThen validationCont validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> andThen validationCont)

        LIST fl fi ->
            LIST fl (\uidxs -> fi uidxs |> andThen validationCont)

        FAIL failState ->
            FAIL failState

        SUCCESS successState ->
            validationCont successState.output


makeList : (FieldList x -> field) -> Validation error x output -> List UniqueIndex -> Validation error field (List output)
makeList fieldListF validation uidxs =
    case uidxs of
        [] ->
            SUCCESS <| SuccessState []

        uidx :: rest ->
            let
                mappedValidation =
                    validation |> mapField (\x -> Form.Types.listField fieldListF uidx x)
            in
            SUCCESS (SuccessState (::))
                |> andMap mappedValidation
                |> andMap (makeList fieldListF validation rest)


andMap : Validation error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMap validation validationF =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> andMap (cont s) validationF)

        LIST fl fi ->
            LIST fl (\uidxs -> andMap (fi uidxs) validationF)

        FAIL failState1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (FAIL failState1) (cont s))

                LIST fl fi ->
                    LIST fl (\uidxs -> fi uidxs |> andMap (FAIL failState1))

                FAIL failState2 ->
                    FAIL <|
                        FailState
                            (failState1.errors |> Form.Types.merge failState2.errors)

                SUCCESS successState ->
                    FAIL <|
                        failState1

        SUCCESS successState1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (SUCCESS successState1) (cont s))

                LIST fl fi ->
                    LIST fl (\uidxs -> fi uidxs |> andMap (SUCCESS successState1))

                FAIL failState ->
                    FAIL <|
                        failState

                SUCCESS successState2 ->
                    SUCCESS <|
                        SuccessState
                            (successState2.output successState1.output)


succeed : output -> Validation error field output
succeed output =
    SUCCESS <| SuccessState output


failure : (Field a -> field) -> error -> Validation error field output
failure fieldF error =
    FAIL <| FailState <| Map.singleton (fieldF Form.Types.Field) (FailCell (Just error))


validate : Form error field output -> Form error field output
validate form =
    let
        validationResult =
            validateHelper form form.validation |> Debug.log "asd"
    in
    case validationResult of
        Succeeded successState ->
            { form | output = Just successState.output } |> clearErrors

        Failed failState ->
            { form | output = Nothing } |> clearErrors |> setErrors (failState.errors |> Map.toList)


clearErrors : Form error field output -> Form error field output
clearErrors form =
    { form | values = form.values |> Form.FieldIndex.map (\state -> { state | error = Nothing }) }


setErrors : List ( field, FailCell error ) -> Form error field output -> Form error field output
setErrors errors form =
    case errors of
        [] ->
            form

        ( field, failCell ) :: rest ->
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

                newValues =
                    case newForm.values |> Form.FieldIndex.get fieldIndex of
                        Nothing ->
                            newForm.values |> Form.FieldIndex.set fieldIndex { value = FVEmpty, error = failCell.error }

                        Just state ->
                            newForm.values |> Form.FieldIndex.set fieldIndex { state | error = failCell.error }
            in
            setErrors rest { newForm | values = newValues }


validateHelper : Form error field output -> Validation error field output -> ValidationResult error field output
validateHelper form validation =
    case validation of
        STR fieldF cont ->
            let
                mFieldIndex =
                    form.fieldIndexes |> Map.get (fieldF Form.Types.Field)

                stringValue =
                    case mFieldIndex of
                        Nothing ->
                            ""

                        Just fi ->
                            case Form.FieldIndex.get fi form.values of
                                Nothing ->
                                    ""

                                Just state ->
                                    state.value |> Form.Types.asString |> Maybe.withDefault ""
            in
            validateHelper form <| cont stringValue

        LIST fl contI ->
            let
                mFieldIndex =
                    form.fieldIndexes |> Map.get fl

                uniqueIndexes =
                    case mFieldIndex of
                        Nothing ->
                            []

                        Just fi ->
                            case Form.FieldIndex.get fi form.listIndexes of
                                Nothing ->
                                    []

                                Just li ->
                                    li |> Map.toList |> List.map Tuple.first
            in
            validateHelper form <| contI uniqueIndexes

        FAIL failState ->
            let
                _ =
                    Debug.log "fs" failState
            in
            failState |> fromFailState

        SUCCESS successState ->
            successState |> fromSuccessState
