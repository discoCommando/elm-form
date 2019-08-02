module Form.Validation exposing (andMap, andThen, failure, fromList, fromNested, fromString, int, lazy, makeList, map, mapField, optional, string, succeed)

import Form
    exposing
        ( FailCell
        , FailState
        , Field
        , FieldList
        , FieldNested
        , SuccessState
        , Validation(..)
        , field
        , mapFailState
        , merge
        )
import Form.Map as Map
import Index.UniqueIndex exposing (UniqueIndex)


string : String -> Result () String
string s =
    case s of
        "" ->
            Err ()

        ss ->
            Ok ss


int : String -> Result () Int
int =
    String.toInt >> Result.fromMaybe ()


optional : (String -> Result error output) -> (String -> Result error (Maybe output))
optional f s =
    case s of
        "" ->
            Ok Nothing

        _ ->
            f s |> Result.map Just


fromString : (Field String -> field) -> (String -> Result error output) -> Validation error field output
fromString fieldF strVal =
    V_STR
        (field fieldF)
        (\s ->
            case strVal s of
                Err error ->
                    V_FAIL <| FailState <| Map.singleton (field fieldF) <| FailCell (Just error)

                Ok v ->
                    V_SUCCESS <| SuccessState v
        )


mapField : (field1 -> field2) -> Validation error field1 output -> Validation error field2 output
mapField mapF validation =
    case validation of
        -- PURE output ->
        --     PURE output
        -- ERROR errors ->
        --     ERROR (errors |> List.map (\(field, error) -> (mapF field, error)))
        V_STR f1 cont ->
            V_STR (f1 |> mapF) (\s -> cont s |> mapField mapF)

        -- V_LIST f1i cont ->
        --     V_LIST (\i f -> f1i i f |> mapF) (\ls -> cont ls |> mapField mapF)
        V_LIST fl fi ->
            V_LIST (fl |> mapF) (\uidxs -> fi uidxs |> mapField mapF)

        -- INPROGRESS errors rest ->
        --     INPROGRESS (errors |> List.map (Tuple.mapFirst mapF)) (rest |> mapField mapF)
        V_FAIL failState ->
            V_FAIL <| mapFailState mapF failState

        V_SUCCESS successState ->
            V_SUCCESS successState

        V_LAZY f ->
            V_LAZY (f >> mapField mapF)


fromNested : (FieldNested x -> field) -> Validation error x output -> Validation error field output
fromNested fieldNF validation =
    mapField (\x -> Form.fieldNestedNotOpaque fieldNF x) validation


fromList : (FieldList x -> field) -> Validation error x output -> Validation error field (List output)
fromList fieldListF validation =
    V_LIST (Form.listOpaque fieldListF)
        (makeList fieldListF validation)


map : (o1 -> o2) -> Validation error field o1 -> Validation error field o2
map f validation =
    case validation of
        V_STR f1 cont ->
            V_STR f1 (\s -> cont s |> map f)

        V_LIST fl fi ->
            V_LIST fl (\uidxs -> fi uidxs |> map f)

        V_FAIL failState ->
            V_FAIL failState

        V_SUCCESS successState ->
            V_SUCCESS <| { output = successState.output |> f }

        V_LAZY f_ ->
            V_LAZY (f_ >> map f)


andThen : (output1 -> Validation error field output2) -> Validation error field output1 -> Validation error field output2
andThen validationCont validation =
    case validation of
        V_STR f1 cont ->
            V_STR f1 (\s -> cont s |> andThen validationCont)

        V_LIST fl fi ->
            V_LIST fl (\uidxs -> fi uidxs |> andThen validationCont)

        V_FAIL failState ->
            V_FAIL failState

        V_SUCCESS successState ->
            validationCont successState.output

        V_LAZY f ->
            V_LAZY (f >> andThen validationCont)


makeList : (FieldList x -> field) -> Validation error x output -> List UniqueIndex -> Validation error field (List output)
makeList fieldListF validation uidxs =
    case uidxs of
        [] ->
            V_SUCCESS <| SuccessState []

        uidx :: rest ->
            let
                mappedValidation =
                    validation |> mapField (\x -> Form.listField fieldListF uidx x)
            in
            V_SUCCESS (SuccessState (::))
                |> andMap mappedValidation
                |> andMap (makeList fieldListF validation rest)


andMap : Validation error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMap validation validationF =
    case validation of
        V_STR f1 cont ->
            V_STR f1 (\s -> andMap (cont s) validationF)

        V_LIST fl fi ->
            V_LIST fl (\uidxs -> andMap (fi uidxs) validationF)

        V_FAIL failState1 ->
            case validationF of
                V_STR f1 cont ->
                    V_STR f1 (\s -> andMap (V_FAIL failState1) (cont s))

                V_LIST fl fi ->
                    V_LIST fl (\uidxs -> fi uidxs |> andMap (V_FAIL failState1))

                V_FAIL failState2 ->
                    V_FAIL <|
                        FailState
                            (failState1.errors |> merge failState2.errors)

                V_SUCCESS _ ->
                    V_FAIL <|
                        failState1

                V_LAZY f ->
                    V_LAZY (f >> andMap (V_FAIL failState1))

        V_SUCCESS successState1 ->
            case validationF of
                V_STR f1 cont ->
                    V_STR f1 (\s -> andMap (V_SUCCESS successState1) (cont s))

                V_LIST fl fi ->
                    V_LIST fl (\uidxs -> fi uidxs |> andMap (V_SUCCESS successState1))

                V_FAIL failState ->
                    V_FAIL <|
                        failState

                V_SUCCESS successState2 ->
                    V_SUCCESS <|
                        SuccessState
                            (successState2.output successState1.output)

                V_LAZY f ->
                    V_LAZY (f >> andMap (V_SUCCESS successState1))

        V_LAZY f ->
            V_LAZY (f >> (\v_ -> andMap v_ validationF))


succeed : output -> Validation error field output
succeed output =
    V_SUCCESS <| SuccessState output


failure : (Field a -> field) -> error -> Validation error field output
failure fieldF error =
    V_FAIL <| FailState <| Map.singleton (fieldF Form.Field) (FailCell (Just error))


lazy : (() -> Validation error field output) -> Validation error field output
lazy =
    V_LAZY
