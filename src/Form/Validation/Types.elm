module Form.Validation.Types exposing (..)

import Form.Types exposing (Field)


type Validation error field output
    = STR (Field String -> field) (String -> Validation error field output)
      -- | LIST (Int -> Field String -> field) (List String -> Validation error field output)
    | LIST (Int -> Validation error field output)
      -- | INPROGRESS (List ( field, error )) (Validation error field output)
    | FAIL (List ( field, error ))
    | SUCCESS output


fromString : (Field String -> field) -> (String -> Result error output) -> Validation error field output
fromString fieldF strVal =
    STR fieldF
        (\s ->
            case strVal s of
                Err error ->
                    FAIL [ ( fieldF Form.Types.Field, error ) ]

                Ok v ->
                    SUCCESS v
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
        FAIL errors ->
            FAIL (errors |> List.map (Tuple.mapFirst mapF))

        SUCCESS o ->
            SUCCESS o


fromNested : (x -> field) -> Validation error x output -> Validation error field output
fromNested =
    mapField


fromList : (Int -> Validation error field output) -> Validation error field (List output)
fromList validationI =
    LIST
        (\length ->
            List.range 0 (length - 1)
                |> List.reverse
                |> List.foldl
                    (\i v ->
                        v
                            |> andThen
                                (\list ->
                                    validationI i
                                        |> andThen
                                            (\res ->
                                                SUCCESS (res :: list)
                                            )
                                )
                    )
                    (SUCCESS [])
        )


andThen : (output1 -> Validation error field output2) -> Validation error field output1 -> Validation error field output2
andThen validationCont validation =
    let
        andThenRest x =
            case x of
                SUCCESS output ->
                    validationCont output

                _ ->
                    x |> andThen validationCont
    in
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> andThenRest)

        LIST fi ->
            LIST (\length -> fi length |> andThenRest)

        FAIL errors ->
            FAIL errors

        SUCCESS output ->
            validationCont output


andMap : Validation error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMap validation validationF =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> andMap (cont s) validationF)

        LIST fi ->
            LIST (\length -> andMap (fi length) validationF)

        FAIL errors1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (FAIL errors1) (cont s))

                LIST fi ->
                    LIST (\length -> fi length |> andMap (FAIL errors1))

                FAIL errors2 ->
                    FAIL (errors1 ++ errors2)

                SUCCESS output ->
                    FAIL errors1

        SUCCESS o1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (SUCCESS o1) (cont s))

                LIST fi ->
                    LIST (\length -> fi length |> andMap (SUCCESS o1))

                FAIL errors ->
                    FAIL errors

                SUCCESS o2 ->
                    SUCCESS (o2 o1)


succeed : output -> Validation error field output
succeed =
    SUCCESS


failure : (Field a -> field) -> error -> Validation error field output
failure fieldF error =
    FAIL [ ( fieldF Form.Types.Field, error ) ]
