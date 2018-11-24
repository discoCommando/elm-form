module Form.Validation exposing (..)

import Form.Map as Map
import Form.Types exposing (FailState, Field, Fields, Validation(..))
import Form.Validation.Types exposing (..)


string : String -> Result error String
string =
    Ok


int : String -> Result () Int
int =
    String.toInt >> Result.mapError (\_ -> ())


fromString : (Field String -> field) -> (String -> Result error output) -> Validation error field output
fromString fieldF strVal =
    STR fieldF
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
        STR f1 cont ->
            STR (f1 >> mapF) (\s -> cont s |> mapField mapF)

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
        (\length -> makeList validationI length)


map : (o1 -> o2) -> Validation error field o1 -> Validation error field o2
map f validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> map f)

        LIST fi ->
            LIST (\length -> fi length |> map f)

        FAIL failState ->
            FAIL failState

        SUCCESS fs o ->
            SUCCESS fs (o |> f)


andThen : (output1 -> Validation error field output2) -> Validation error field output1 -> Validation error field output2
andThen validationCont validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> andThen validationCont)

        LIST fi ->
            LIST (\length -> fi length |> andThen validationCont)

        FAIL failState ->
            FAIL failState

        SUCCESS fs output ->
            validationCont output |> addSucceededFields fs


addSucceededFields : List field -> Validation error field output -> Validation error field output
addSucceededFields fs validation =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> addSucceededFields fs)

        LIST fi ->
            LIST (\length -> fi length |> addSucceededFields fs)

        FAIL { succeeded, errors, notFounds } ->
            FAIL <| FailState (fs ++ succeeded) errors notFounds

        SUCCESS fs2 output ->
            SUCCESS (fs ++ fs2) output


makeList : (Int -> Validation error field output) -> Int -> Validation error field (List output)
makeList validationI i =
    if i < 0 then
        SUCCESS [] []
    else
        case validationI i of
            FAIL { succeeded, errors, notFounds } ->
                case ( succeeded, errors ) of
                    ( [], [] ) ->
                        makeList validationI (i - 1)

                    _ ->
                        validationI i
                            |> andThen
                                (\o ->
                                    makeList validationI (i - 1)
                                        |> map
                                            (\os ->
                                                o :: os
                                            )
                                )

            x ->
                x
                    |> andThen
                        (\o ->
                            makeList validationI (i - 1)
                                |> map
                                    (\os ->
                                        o :: os
                                    )
                        )


andMap : Validation error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMap validation validationF =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> andMap (cont s) validationF)

        LIST fi ->
            LIST (\length -> andMap (fi length) validationF)

        FAIL failState1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (FAIL failState1) (cont s))

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
                STR f1 cont ->
                    STR f1 (\s -> andMap (SUCCESS fs1 o1) (cont s))

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
            validateHelper False fields validation

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


validateHelper : Bool -> Fields error field -> Validation error field output -> ValidationResult error field output
validateHelper inList fields validation =
    case validation of
        STR fieldF cont ->
            case inList of
                False ->
                    validateHelper inList
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
                    validateHelper inList
                        fields
                        (cont
                            (fields
                                |> Map.get (fieldF Form.Types.Field)
                                |> Maybe.andThen (.value >> Form.Types.asString)
                            )
                        )

        LIST contI ->
            validateHelper True fields <| contI (Map.length fields)

        FAIL failState ->
            let
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
