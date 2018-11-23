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


type BasicField
    = S String
    | B Bool


type Validation3 error field output
    = STR (Field String -> field) (String -> Validation3 error field output)
      -- | LIST (Int -> Field String -> field) (List String -> Validation3 error field output)
    | LIST (Int -> Validation3 error field output)
    | INPROGRESS (List ( field, error )) (Validation3 error field output)
    | FAIL (List ( field, error ))
    | SUCCESS output



-- | NESTEDLIST (Int -> Validation3 error field output)
-- | NESTEDLIST (Int -> Validation3 error field output)
-- | NESTEDLIST (Int -> field) ()
-- | PURE output
-- | ERROR (List (field, error))
-- type ValidationRes2 error field output
--     = InProgress (List (field, error)) (Validation3 error field output)
--     | Fail (List (field, error))
--     | Succ output


type Simple
    = Field1 (Field String)


type alias SimpleOutput =
    { field1 : Int
    }


str3 : (Field String -> field) -> (String -> Result error output) -> Validation3 error field output
str3 fieldF strVal =
    STR fieldF
        (\s ->
            case strVal s of
                Err error ->
                    FAIL [ ( fieldF Field, error ) ]

                Ok v ->
                    SUCCESS v
        )


mapValidationField : (field1 -> field2) -> Validation3 error field1 output -> Validation3 error field2 output
mapValidationField mapF validation =
    case validation of
        -- PURE output ->
        --     PURE output
        -- ERROR errors ->
        --     ERROR (errors |> List.map (\(field, error) -> (mapF field, error)))
        STR f1 cont ->
            STR (f1 >> mapF) (\s -> cont s |> mapValidationField mapF)

        -- LIST f1i cont ->
        --     LIST (\i f -> f1i i f |> mapF) (\ls -> cont ls |> mapValidationField mapF)
        LIST fi ->
            LIST (\length -> fi length |> mapValidationField mapF)

        INPROGRESS errors rest ->
            INPROGRESS (errors |> List.map (Tuple.mapFirst mapF)) (rest |> mapValidationField mapF)

        FAIL errors ->
            FAIL (errors |> List.map (Tuple.mapFirst mapF))

        SUCCESS o ->
            SUCCESS o



-- mapValidationResField : (field1 -> field2) -> ValidationRes2 error field1 output -> ValidationRes2 error field2 output
-- mapValidationResField mapF validationRes =
--     case validationRes of
--         InProgress errors rest ->
--             InProgress (errors |> List.map (Tuple.mapFirst mapF)) (rest |> mapValidationField mapF)
--         Fail errors ->
--             Fail (errors |> List.map (Tuple.mapFirst mapF))
--         Succ x ->
--             Succ x


nested3 : (x -> field) -> Validation3 error x output -> Validation3 error field output
nested3 =
    mapValidationField



-- list3 : (Int -> Field String -> field) -> (String -> Result error output) -> Validation3 error field (List output)
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

list333 : (Int -> Validation3 error field output) -> Validation3 error field (List output)
list333 validationI = 
    LIST
        (\length ->
            List.range 0 (length - 1)
                |> List.reverse
                |> List.foldl
                    (\i v ->
                        v
                            |> andThenV3
                                (\list ->
                                    validationI i
                                        |> andThenV3
                                            (\res ->
                                                SUCCESS (res :: list)
                                            )
                                )
                    )
                    (SUCCESS [])
        )


-- list33 : (Int -> x -> field) -> Validation3 error x output -> Validation3 error field (List output)
-- list33 fieldIF validation =
--     LIST
--         (\length ->
--             List.range 0 (length - 1)
--                 |> List.reverse
--                 |> List.foldl
--                     (\i v ->
--                         v
--                             |> andThenV3
--                                 (\list ->
--                                     validation
--                                         |> mapValidationField (fieldIF i)
--                                         |> andThenV3
--                                             (\res ->
--                                                 SUCCESS (res :: list)
--                                             )
--                                 )
--                     )
--                     (SUCCESS [])
--         )



-- nestedList3 : (Int -> x -> field) -> Validation3 error x output -> Validation3 error field (List output)
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


andThenV3 : (output1 -> Validation3 error field output2) -> Validation3 error field output1 -> Validation3 error field output2
andThenV3 validationCont validation =
    let
        andThenRest x =
            case x of
                SUCCESS output ->
                    validationCont output

                _ ->
                    x |> andThenV3 validationCont
    in
    case validation of
        STR f1 cont ->
            STR f1 (\s -> cont s |> andThenRest)

        -- LIST f1i cont ->
        --     LIST f1i (\ls -> cont ls |> andThenRest)
        LIST fi ->
            LIST (\length -> fi length |> andThenRest)

        INPROGRESS errors rest ->
            INPROGRESS errors (rest |> andThenV3 validationCont)

        FAIL errors ->
            FAIL errors

        SUCCESS output ->
            validationCont output


andMapV3 : Validation3 error field output1 -> Validation3 error field (output1 -> output2) -> Validation3 error field output2
andMapV3 validation validationF =
    case validation of
        STR f1 cont ->
            STR f1 (\s -> andMapV3 (cont s) validationF)

        -- LIST f1i cont ->
        --     LIST f1i (\ls -> andMapV3 (cont ls) validationF)
        LIST fi ->
            LIST (\length -> andMapV3 (fi length) validationF)

        INPROGRESS errors rest ->
            INPROGRESS errors (andMapV3 rest validationF)

        FAIL errors1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMapV3 (FAIL errors1) (cont s))

                -- LIST f1i cont ->
                --     LIST f1i (\ls -> andMapV3 (FAIL errors1) (cont ls))
                LIST fi ->
                    LIST (\length -> fi length |> andMapV3 (FAIL errors1))

                INPROGRESS errors2 rest ->
                    INPROGRESS errors2 (andMapV3 (FAIL errors1) rest)

                FAIL errors2 ->
                    FAIL (errors1 ++ errors2)

                SUCCESS output ->
                    FAIL errors1

        SUCCESS o1 ->
            case validationF of
                STR f1 cont ->
                    STR f1 (\s -> andMapV3 (SUCCESS o1) (cont s))

                -- LIST f1i cont ->
                --     LIST f1i (\ls -> andMapV3 (SUCCESS o1) (cont ls))
                LIST fi ->
                    LIST (\length -> fi length |> andMapV3 (SUCCESS o1))

                INPROGRESS errors rest ->
                    INPROGRESS errors (andMapV3 (SUCCESS o1) rest)

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
-- validation |> mapValidationField (\x i -> fieldFi i x)
-- list3String : (Int -> Field String -> field) -> (String -> Result error output)
-- val1 : Validation3 field output
-- val1
-- type alias Validation2 error field output =
--     (Fields error field, )
