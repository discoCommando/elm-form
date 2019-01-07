module Form exposing (..)

-- COMPOSABLE
-- TYPE SAFE

import Form.FieldState exposing (FieldState)
import Form.Map as Map exposing (Map)
import Index.FieldIndex as FieldIndex exposing (..)
import Index.FieldIndexDict as FieldIndexDict exposing (..)
import Index.UniqueIndex as UniqueIndex exposing (..)
import Index.UniqueIndexDict as UniqueIndexDict exposing (..)


type Validation error field output
    = V_STR field (String -> Validation error field output)
    | V_LIST field (List UniqueIndex -> Validation error field output)
    | V_FAIL (FailState error field)
    | V_SUCCESS (SuccessState output)


type Field a
    = Field


type FieldList a
    = OpaqueList
    | WithIndex UniqueIndex a


type FieldNested a
    = OpaqueNested
    | WithValue a


listOpaque : (FieldList a -> field) -> field
listOpaque listF =
    listF OpaqueList


listField : (FieldList a -> field) -> UniqueIndex -> a -> field
listField listF i a =
    listF (WithIndex i a)


field : (Field a -> field) -> field
field fieldF =
    fieldF Field


fieldNested : (FieldNested a -> field) -> field
fieldNested fieldF =
    fieldF OpaqueNested


fieldNestedNotOpaque : (FieldNested a -> field) -> a -> field
fieldNestedNotOpaque fieldF a =
    fieldF (WithValue a)


type alias Form error field output =
    { fieldIndexes : Map field FieldIndex
    , listIndexes : FieldIndexDict (UniqueIndexDict (FieldIndexDict ()))
    , values : FieldIndexDict (FieldState error)
    , submitted : Bool
    , validation : Validation error field output
    , output : Maybe output
    , fieldIndexToUse : FieldIndex
    , uniqueIndexToUse : UniqueIndex
    }


form : Validation field error output -> Form field error output
form validation =
    { fieldIndexes = Map.empty
    , listIndexes = FieldIndexDict.empty
    , values = FieldIndexDict.empty
    , submitted = False
    , validation = validation
    , output = Nothing
    , fieldIndexToUse = FieldIndex.create
    , uniqueIndexToUse = UniqueIndex.create
    }
        |> validate


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
    { form | values = form.values |> FieldIndexDict.map (\state -> { state | error = Nothing }) }


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
                                , fieldIndexToUse = form.fieldIndexToUse |> FieldIndex.next
                              }
                            )

                        Just fi ->
                            ( fi, form )

                newValues =
                    case newForm.values |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            newForm.values |> FieldIndexDict.set fieldIndex { value = Form.FieldState.FVEmpty, error = failCell.error }

                        Just state ->
                            newForm.values |> FieldIndexDict.set fieldIndex { state | error = failCell.error }
            in
            setErrors rest { newForm | values = newValues }


validateHelper : Form error field output -> Validation error field output -> ValidationResult error field output
validateHelper form validation =
    case validation of
        V_STR field cont ->
            let
                mFieldIndex =
                    form.fieldIndexes |> Map.get field

                stringValue =
                    case mFieldIndex of
                        Nothing ->
                            ""

                        Just fi ->
                            case FieldIndexDict.get fi form.values of
                                Nothing ->
                                    ""

                                Just state ->
                                    state.value |> Form.FieldState.asString |> Maybe.withDefault ""
            in
            validateHelper form <| cont stringValue

        V_LIST fl contI ->
            let
                mFieldIndex =
                    form.fieldIndexes |> Map.get fl

                uniqueIndexes =
                    case mFieldIndex of
                        Nothing ->
                            []

                        Just fi ->
                            case FieldIndexDict.get fi form.listIndexes of
                                Nothing ->
                                    []

                                Just li ->
                                    li |> UniqueIndexDict.keys
            in
            validateHelper form <| contI uniqueIndexes

        V_FAIL failState ->
            let
                _ =
                    Debug.log "fs" failState
            in
            failState |> fromFailState

        V_SUCCESS successState ->
            successState |> fromSuccessState


type alias FailCell error =
    { error : Maybe error
    }


type alias FailState error field =
    { errors : Map field (FailCell error) }


type alias SuccessState output =
    { output : output
    }


type ValidationResult error field output
    = Failed (FailState error field)
    | Succeeded (SuccessState output)


type alias ValidationResultCell error =
    { error : Maybe error
    }


fromFailState : FailState error field -> ValidationResult error field output
fromFailState =
    Failed


fromSuccessState : SuccessState output -> ValidationResult error field output
fromSuccessState =
    Succeeded


mapFailState : (field1 -> field2) -> FailState error field1 -> FailState error field2
mapFailState mapf failState =
    { failState | errors = failState.errors |> Map.mapBoth (\key failCell -> ( key |> mapf, failCell )) }


merge : Map field (FailCell error) -> Map field (FailCell error) -> Map field (FailCell error)
merge fields1 fields2 =
    fields1
        |> Map.foldl
            (\field failCell m ->
                m
                    |> Map.updateWithDefault field
                        (\mfailCell2 ->
                            case mfailCell2 of
                                Nothing ->
                                    failCell

                                Just failCell2 ->
                                    case failCell.error of
                                        Nothing ->
                                            failCell2

                                        _ ->
                                            failCell
                        )
            )
            fields2


type Transaction field
    = T_STR (Field String -> field) String
    | T_ADDROW field (UniqueIndex -> Transaction field)
    | T_REMOVEROW field UniqueIndex
    | T_BATCH (List (Transaction field))
