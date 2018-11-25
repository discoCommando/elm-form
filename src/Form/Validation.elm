module Form.Validation exposing (..)

import Form.Map as Map
import Form.Types exposing (FailState, Field, FieldList, Fields, Validation(..))
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
                    FAIL <| FailState [] [ ( fieldF Form.Types.Field, error ) ]

                Ok v ->
                    SUCCESS [ fieldF Form.Types.Field ] v
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
            LIST (fl |> mapF) (\length -> fi length |> mapField mapF)

        -- INPROGRESS errors rest ->
        --     INPROGRESS (errors |> List.map (Tuple.mapFirst mapF)) (rest |> mapField mapF)
        FAIL { succeeded, errors } ->
            FAIL <| FailState (succeeded |> List.map mapF) (errors |> List.map (Tuple.mapFirst mapF))

        SUCCESS fs o ->
            SUCCESS (fs |> List.map mapF) o


fromNested : (x -> field) -> Validation error x output -> Validation error field output
fromNested =
    mapField


fromListString : (FieldList (Field String) -> field) -> (String -> Result error output) -> Validation error field (List output)
fromListString fieldListF strValidation =
    fromList fieldListF (fromString identity strValidation)


fromList : (FieldList x -> field) -> Validation error x output -> Validation error field (List output)
fromList fieldListF validation =
    LIST (Form.Types.listOpaque fieldListF)
        (\length -> makeList fieldListF validation 0 length)


map : (o1 -> o2) -> Validation error field o1 -> Validation error field o2
map f validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> map f)

        LIST fl fi ->
            LIST fl (\length -> fi length |> map f)

        FAIL failState ->
            FAIL failState

        SUCCESS fs o ->
            SUCCESS fs (o |> f)


andThen : (output1 -> Validation error field output2) -> Validation error field output1 -> Validation error field output2
andThen validationCont validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> andThen validationCont)

        LIST fl fi ->
            LIST fl (\length -> fi length |> andThen validationCont)

        FAIL failState ->
            FAIL failState

        SUCCESS fs output ->
            validationCont output |> addSucceededFields fs


andThenIgnoreNotFounds : (Maybe output -> Validation error field (List output)) -> Validation error field output -> Validation error field (List output)
andThenIgnoreNotFounds validationCont validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> andThenIgnoreNotFounds validationCont)

        LIST fl fi ->
            LIST fl (\length -> fi length |> andThenIgnoreNotFounds validationCont)

        FAIL failState ->
            case ( failState.succeeded, failState.errors ) of
                ( [], [] ) ->
                    validationCont Nothing |> addSucceededFields failState.succeeded

                _ ->
                    FAIL failState

        SUCCESS fs o ->
            validationCont (Just o) |> addSucceededFields fs


addSucceededFields : List field -> Validation error field output -> Validation error field output
addSucceededFields fs validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> addSucceededFields fs)

        LIST fl fi ->
            LIST fl (\length -> fi length |> addSucceededFields fs)

        FAIL { succeeded, errors } ->
            FAIL <| FailState (fs ++ succeeded) errors

        SUCCESS fs2 output ->
            SUCCESS (fs ++ fs2) output


makeList : (FieldList x -> field) -> Validation error x output -> Int -> Int -> Validation error field (List output)
makeList fieldListF validation i max =
    let
        _ =
            Debug.log (toString i) validation
    in
    if i >= max then
        SUCCESS [] []
    else
        let
            mappedValidation =
                validation |> mapField (\x -> Form.Types.listField fieldListF i x)
        in
        SUCCESS [] (::)
            |> andMap mappedValidation
            |> andMap (makeList fieldListF validation (i + 1) max)


andMap : Validation error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMap validation validationF =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> andMap (cont s) validationF)

        LIST fl fi ->
            LIST fl (\length -> andMap (fi length) validationF)

        FAIL failState1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (FAIL failState1) (cont s))

                LIST fl fi ->
                    LIST fl (\length -> fi length |> andMap (FAIL failState1))

                FAIL failState2 ->
                    FAIL <|
                        FailState
                            (failState1.succeeded ++ failState2.succeeded)
                            (failState1.errors ++ failState2.errors)

                SUCCESS fs output ->
                    FAIL <| FailState (failState1.succeeded ++ fs) failState1.errors

        SUCCESS fs1 o1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (SUCCESS fs1 o1) (cont s))

                LIST fl fi ->
                    LIST fl (\length -> fi length |> andMap (SUCCESS fs1 o1))

                FAIL { succeeded, errors } ->
                    FAIL <| FailState (fs1 ++ succeeded) errors

                SUCCESS fs2 o2 ->
                    SUCCESS (fs1 ++ fs2) (o2 o1)


succeed : output -> Validation error field output
succeed =
    SUCCESS []


failure : (Field a -> field) -> error -> Validation error field output
failure fieldF error =
    FAIL <| FailState [] [ ( fieldF Form.Types.Field, error ) ]


validate : Validation error field output -> Fields error field -> ( Fields error field, Maybe output )
validate validation fields =
    let
        validationResult =
            validateHelper fields validation |> Debug.log "asd"

        newFields =
            fields
                |> Map.mapBoth
                    (\k v ->
                        case Map.get k validationResult.fields of
                            Nothing ->
                                ( k, v )

                            Just x ->
                                ( k, { v | error = x } )
                    )
    in
    ( newFields, validationResult.result )


validateHelper : Fields error field -> Validation error field output -> ValidationResult error field output
validateHelper fields validation =
    case validation of
        STR fieldF cont ->
            let
                _ =
                    Debug.log ("STR2 " ++ toString (fieldF Form.Types.Field)) False
            in
            validateHelper
                fields
                (cont
                    (fields
                        |> Map.get (fieldF Form.Types.Field)
                        |> Maybe.andThen (.value >> Form.Types.asString)
                        |> Maybe.withDefault ""
                    )
                )

        LIST fl contI ->
            let
                _ =
                    Debug.log "LIST" fl

                length =
                    fields |> Map.get fl |> Maybe.andThen (.value >> Form.Types.asLength) |> Maybe.withDefault 0
            in
            validateHelper fields <| contI length

        FAIL failState ->
            let
                _ =
                    Debug.log "fs" failState

                succeededMap =
                    failState.succeeded |> List.foldl (\f m -> m |> Map.set f Nothing) Map.empty

                failMap =
                    failState.errors |> List.foldl (\( f, e ) m -> m |> Map.set f (Just e)) Map.empty
            in
            { fields = succeededMap |> Map.mergeWith failMap
            , result = Nothing
            }

        SUCCESS fs output ->
            { fields = fs |> List.foldl (\f m -> m |> Map.set f Nothing) Map.empty
            , result = Just output
            }
