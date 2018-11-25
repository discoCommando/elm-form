module Form.Validation exposing (..)

import Form.Map as Map
import Form.Types exposing (FailState, Field, Fields, Validation(..))
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
    STR False
        fieldF
        (\ms ->
            case ms of
                Nothing ->
                    FAIL <| FailState [] [] [ fieldF Form.Types.Field ]

                Just s ->
                    case strVal s of
                        Err error ->
                            FAIL <| FailState [] [ ( fieldF Form.Types.Field, error ) ] []

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
        STR b f1 cont ->
            STR b (f1 >> mapF) (\s -> cont s |> mapField mapF)

        -- LIST f1i cont ->
        --     LIST (\i f -> f1i i f |> mapF) (\ls -> cont ls |> mapField mapF)
        LIST fi ->
            LIST (\length -> fi length |> mapField mapF)

        -- INPROGRESS errors rest ->
        --     INPROGRESS (errors |> List.map (Tuple.mapFirst mapF)) (rest |> mapField mapF)
        FAIL { succeeded, errors, notFounds } ->
            FAIL <| FailState (succeeded |> List.map mapF) (errors |> List.map (Tuple.mapFirst mapF)) (notFounds |> List.map mapF)

        SUCCESS fs o ->
            SUCCESS (fs |> List.map mapF) o


fromNested : (x -> field) -> Validation error x output -> Validation error field output
fromNested =
    mapField


fromList : (Int -> Validation error field output) -> Validation error field (List output)
fromList validationI =
    LIST
        (\length -> makeList validationI 0 length)


map : (o1 -> o2) -> Validation error field o1 -> Validation error field o2
map f validation =
    case validation of
        STR b f1 cont ->
            STR b f1 (\s -> cont s |> map f)

        LIST fi ->
            LIST (\length -> fi length |> map f)

        FAIL failState ->
            FAIL failState

        SUCCESS fs o ->
            SUCCESS fs (o |> f)


andThen : (output1 -> Validation error field output2) -> Validation error field output1 -> Validation error field output2
andThen validationCont validation =
    case validation of
        STR b f1 cont ->
            STR b f1 (\s -> cont s |> andThen validationCont)

        LIST fi ->
            LIST (\length -> fi length |> andThen validationCont)

        FAIL failState ->
            FAIL failState

        SUCCESS fs output ->
            validationCont output |> addSucceededFields fs


andThenIgnoreNotFounds : (Maybe output -> Validation error field (List output)) -> Validation error field output -> Validation error field (List output)
andThenIgnoreNotFounds validationCont validation =
    case validation of
        STR b f1 cont ->
            STR True f1 (\s -> cont s |> andThenIgnoreNotFounds validationCont)

        LIST fi ->
            LIST (\length -> fi length |> andThenIgnoreNotFounds validationCont)

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
        STR b f1 cont ->
            STR b f1 (\s -> cont s |> addSucceededFields fs)

        LIST fi ->
            LIST (\length -> fi length |> addSucceededFields fs)

        FAIL { succeeded, errors, notFounds } ->
            FAIL <| FailState (fs ++ succeeded) errors notFounds

        SUCCESS fs2 output ->
            SUCCESS (fs ++ fs2) output


makeList : (Int -> Validation error field output) -> Int -> Int -> Validation error field (List output)
makeList validationI i max =
    let
        _ =
            Debug.log (toString i) (validationI i)
    in
    if i >= max then
        SUCCESS [] []
    else
        let
            next x =
                x
                    |> andThenIgnoreNotFounds
                        (\mo ->
                            case mo of
                                Nothing ->
                                    makeList validationI (i + 1) max

                                Just o ->
                                    makeList validationI (i + 1) max
                                        |> map
                                            (\os ->
                                                o :: os
                                            )
                        )
        in
        case validationI i of
            FAIL { succeeded, errors, notFounds } ->
                -- case (succeeded, errors) of
                --     ([], []) ->
                --         makeList validationI (i + 1) max
                --     _ ->
                --         next <| validationI i
                SUCCESS [] []

            STR False f cont ->
                next <| STR True f cont

            _ ->
                next <| validationI i


andMap : Validation error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMap validation validationF =
    case validation of
        STR b f1 cont ->
            STR b f1 (\s -> andMap (cont s) validationF)

        LIST fi ->
            LIST (\length -> andMap (fi length) validationF)

        FAIL failState1 ->
            case validationF of
                STR b f1 cont ->
                    STR b f1 (\s -> andMap (FAIL failState1) (cont s))

                LIST fi ->
                    LIST (\length -> fi length |> andMap (FAIL failState1))

                FAIL failState2 ->
                    FAIL <|
                        FailState
                            (failState1.succeeded ++ failState2.succeeded)
                            (failState1.errors ++ failState2.errors)
                            (failState1.notFounds ++ failState2.notFounds)

                SUCCESS fs output ->
                    FAIL <| FailState (failState1.succeeded ++ fs) failState1.errors failState1.notFounds

        SUCCESS fs1 o1 ->
            case validationF of
                STR b f1 cont ->
                    STR b f1 (\s -> andMap (SUCCESS fs1 o1) (cont s))

                LIST fi ->
                    LIST (\length -> fi length |> andMap (SUCCESS fs1 o1))

                FAIL { succeeded, errors, notFounds } ->
                    FAIL <| FailState (fs1 ++ succeeded) errors notFounds

                SUCCESS fs2 o2 ->
                    SUCCESS (fs1 ++ fs2) (o2 o1)


succeed : output -> Validation error field output
succeed =
    SUCCESS []


failure : (Field a -> field) -> error -> Validation error field output
failure fieldF error =
    FAIL <| FailState [] [ ( fieldF Form.Types.Field, error ) ] []


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
        STR inList fieldF cont ->
            let
                _ =
                    Debug.log ("STR2 " ++ toString (fieldF Form.Types.Field)) inList
            in
            case inList of
                False ->
                    validateHelper
                        fields
                        (cont
                            (fields
                                |> Map.get (fieldF Form.Types.Field)
                                |> Maybe.andThen (.value >> Form.Types.asString)
                                |> Maybe.withDefault ""
                                |> Just
                            )
                        )

                True ->
                    validateHelper
                        fields
                        (cont
                            (fields
                                |> Map.get (fieldF Form.Types.Field)
                                |> Maybe.andThen (.value >> Form.Types.asString)
                            )
                        )

        LIST contI ->
            let
                _ =
                    Debug.log "LIST" ""
            in
            validateHelper fields <| contI (Map.length fields)

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
