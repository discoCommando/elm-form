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



-- mapValidationResField : (field1 -> field2) -> ValidationRes2 error field1 output -> ValidationRes2 error field2 output
-- mapValidationResField mapF validationRes =
--     case validationRes of
--         InProgress errors rest ->
--             InProgress (errors |> List.map (Tuple.mapFirst mapF)) (rest |> mapField mapF)
--         Fail errors ->
--             Fail (errors |> List.map (Tuple.mapFirst mapF))
--         Succ x ->
--             Succ x


fromNested : (x -> field) -> Validation error x output -> Validation error field output
fromNested =
    mapField



-- list3 : (Int -> Field String -> field) -> (String -> Result error output) -> Validation error field (List output)
-- list3 fieldFI validation =
--     LIST fieldFI (\ls ->
--         let
--             validatedFields = ls |> List.indexedMap (\i s -> (fieldFI i Field, validation s))
--             errors = validatedFields |> List.filterMap (\(field, x) ->
--                 case x of
--                     Err error ->
--                         Just (field, error)
--                     _ ->
--                         Nothing)
--         in
--         case errors of
--             [] ->
--                 validatedFields
--                 |> List.filterMap (Tuple.second >> Result.toMaybe)
--                 |> SUCCESS
--             _ ->
--                 errors
--                 |> FAIL
--     )
-- list3 : (Int -> Field String -> field)

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


-- list33 : (Int -> x -> field) -> Validation error x output -> Validation error field (List output)
-- list33 fieldIF validation =
--     LIST
--         (\length ->
--             List.range 0 (length - 1)
--                 |> List.reverse
--                 |> List.foldl
--                     (\i v ->
--                         v
--                             |> andThen
--                                 (\list ->
--                                     validation
--                                         |> mapField (fieldIF i)
--                                         |> andThen
--                                             (\res ->
--                                                 SUCCESS (res :: list)
--                                             )
--                                 )
--                     )
--                     (SUCCESS [])
--         )



-- nestedList3 : (Int -> x -> field) -> Validation error x output -> Validation error field (List output)
-- nestedList3 fieldFi validation =
--     case validation of
--         -- PURE output ->
--         --     PURE [output]
--         -- ERROR errors ->
--         --     LIST (\i fs -> fieldFi i fs)
--         STR f1 cont ->
--             list3 (\i fs -> fieldFi i (f1 fs)) (\s -> cont s |> nestedList3 fieldFi)
--         LIST f1 cont ->
--             NESTEDLIST (\length -> )


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

        -- LIST f1i cont ->
        --     LIST f1i (\ls -> cont ls |> andThenRest)
        LIST fi ->
            LIST (\length -> fi length |> andThenRest)

        -- INPROGRESS errors rest ->
        --     INPROGRESS errors (rest |> andThen validationCont)

        FAIL errors ->
            FAIL errors

        SUCCESS output ->
            validationCont output


andMap : Validation error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMap validation validationF =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> andMap (cont s) validationF)

        -- LIST f1i cont ->
        --     LIST f1i (\ls -> andMap (cont ls) validationF)
        LIST fi ->
            LIST (\length -> andMap (fi length) validationF)

        -- INPROGRESS errors rest ->
        --     INPROGRESS errors (andMap rest validationF)

        FAIL errors1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (FAIL errors1) (cont s))

                -- LIST f1i cont ->
                --     LIST f1i (\ls -> andMap (FAIL errors1) (cont ls))
                LIST fi ->
                    LIST (\length -> fi length |> andMap (FAIL errors1))

                -- INPROGRESS errors2 rest ->
                --     INPROGRESS errors2 (andMap (FAIL errors1) rest)

                FAIL errors2 ->
                    FAIL (errors1 ++ errors2)

                SUCCESS output ->
                    FAIL errors1

        SUCCESS o1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMap (SUCCESS o1) (cont s))

                -- LIST f1i cont ->
                --     LIST f1i (\ls -> andMap (SUCCESS o1) (cont ls))
                LIST fi ->
                    LIST (\length -> fi length |> andMap (SUCCESS o1))

                -- INPROGRESS errors rest ->
                --     INPROGRESS errors (andMap (SUCCESS o1) rest)

                FAIL errors ->
                    FAIL errors

                SUCCESS o2 ->
                    SUCCESS (o2 o1)



--             )
-- NESTEDLIST ( )
-- case validation of
--     STR f1 cont ->
--         list3 (\i fs -> fieldFi i (f1 fs)) (\s -> cont s |> nestedList3 fieldFi)
--     LIST f1i cont ->
--         NESTEDLIST (LIST (\i1 f i2 -> fieldFi i2 (f1i i1 f)) (\ls -> cont ls |> nestedList3 ))
-- validation |> mapField (\x i -> fieldFi i x)
-- list3String : (Int -> Field String -> field) -> (String -> Result error output)
-- val1 : Validation field output
-- val1
-- type alias Validation2 error field output =
--     (Fields error field, )
