module Form.Validation exposing
    ( ..
    )

import Form.CommonError exposing (CommonError(..))
import Form.Field as Field
import Form.FieldState as FieldState
import Form.Get as Get
import Form.Map as Map exposing (Map)
import Form.Type
import Index.FieldIndex as FieldIndex
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex exposing (UniqueIndex)



-- few states
-- field not edited
-- form not submitted -> no error
-- form submitted -> show error
-- field edited
-- form submitted
-- form not submitted


type Validation error field output
    = V_ACTION (ValidationAction error field output)
    | V_RESULT (ValidationResult error field output)


type ValidationAction error field output
    = VA_STR field (Get.ValueState String -> Validation error field output)
    | VA_BOOL field (Get.ValueState Bool -> Validation error field output)
    | VA_LIST field (List UniqueIndex -> Validation error field output)
    | VA_LAZY (() -> Validation error field output)


type FailCell error =
    Error error 
    | Loading 


type alias FailState error field =
    { errors : Map field (FailCell error) }


type alias SuccessState output =
    { output : output }


type ValidationResult error field output
    = VR_FAIL (FailState error field)
    | VR_SUCCESS (SuccessState output)

type Free error field a 
    = Impure (Validation error field (Free error field a))
    | Pure a
     
andMapFree : 


type alias Form error field output submitted =
    Form.Type.Form error field output (Validation error field output) submitted


string : String -> Result error String 
string = Ok 

int : String -> Result CommonError Int
int =
    String.toInt >> Result.fromMaybe (Form.CommonError.Custom "Field must be a number")


optional : (String -> Result error output) -> (String -> Result error (Maybe output))
optional f s =
    case s of 
        "" -> 
            Ok Nothing 

        _ -> 
            f s |> Result.map Just


required : (String -> Result CommonError output) -> (String -> Result CommonError output)
required f s =
    case s of 
        "" -> 
            Err NoInput

        _ ->  
            f s


fromString : (Field.Value String -> field) -> (String -> Result error output) -> Validation error field output
fromString fieldF parseFunction =
    V_ACTION <|
        VA_STR
            (fieldF Field.Value)
            (\gs ->
                let 
                    s = gs |> Get.toMaybe |> Maybe.withDefault ""
                in
                case parseFunction s of
                    Err error ->
                        failure fieldF error

                    Ok v ->
                        succeed v
            )


fromBool : (Field.Value Bool -> field) -> (Bool -> Result error output) -> Validation error field output
fromBool fieldF parseFunction =
    V_ACTION <|
        VA_BOOL
            (fieldF Field.Value)
            (\gb ->
                let 
                    b = gb |> Get.toMaybe |> Maybe.withDefault False
                in
                case parseFunction b of
                    Err error ->
                        failure fieldF error

                    Ok v ->
                        succeed v
            )

isTrue : (Field.Value Bool -> field) -> Validation CommonError field ()
isTrue fieldF =
    fromBool fieldF
        (\b ->
            if b then
                Ok ()

            else
                Err NoInput
        )


fromNested : (Field.Nested x -> field) -> Validation error x output -> Validation error field output
fromNested fieldNF validation =
    mapField (\x -> fieldNF <| Field.WithValue x) validation


fromList : (Field.List x -> field) -> Validation error x output -> Validation error field (List output)
fromList fieldListF validation =
    V_ACTION <|
        VA_LIST (fieldListF Field.OpaqueList)
            (makeList fieldListF validation)


mapField : (field1 -> field2) -> Validation error field1 output -> Validation error field2 output
mapField mapF validation =
    case validation of
        V_ACTION va ->
            V_ACTION <|
                case va of
                    VA_STR f1 cont ->
                        VA_STR (f1 |> mapF) (\s -> cont s |> mapField mapF)

                    VA_BOOL f1 cont ->
                        VA_BOOL (f1 |> mapF) (\s -> cont s |> mapField mapF)

                    VA_LIST fl fi ->
                        VA_LIST (fl |> mapF) (\uidxs -> fi uidxs |> mapField mapF)

                    VA_LAZY f ->
                        VA_LAZY (f >> mapField mapF)

        V_RESULT vr ->
            V_RESULT <|
                case vr of
                    VR_FAIL failState ->
                        VR_FAIL <| mapFailState mapF failState

                    VR_SUCCESS successState ->
                        VR_SUCCESS successState


mapVAVs : (Validation error1 field1 o1 -> Validation error2 field1 o2) -> ValidationAction error1 field1 o1 -> ValidationAction error2 field1 o2
mapVAVs f validationAction =
    case validationAction of
        VA_STR f1 cont ->
            VA_STR f1 (cont >> f)

        VA_BOOL f1 cont ->
            VA_BOOL f1 (cont >> f)

        VA_LIST fl fi ->
            VA_LIST fl (fi >> f)

        VA_LAZY f_ ->
            VA_LAZY (f_ >> f)


mapVA : (o1 -> o2) -> ValidationAction error field o1 -> ValidationAction error field o2
mapVA f =
    mapvavs (map f)


mapvr : (o1 -> o2) -> validationresult error field o1 -> validationresult error field o2
mapvr f validationresult =
    case validationresult of
        Vr_success { output } ->
            vr_success { output = f output }

        Vr_fail failstate ->
            vr_fail failstate


map : (o1 -> o2) -> Validation error field o1 -> Validation error field o2
map f validation =
    case validation of
        V_ACTION va ->
            V_ACTION (mapVA f va)

        V_RESULT vr ->
            V_RESULT (mapVR f vr)



andThenVA : (output1 -> Validation error field output2) -> ValidationAction error field output1 -> ValidationAction error field output2
andThenVA f =
    mapVAVs (andThen f)


andThenVR : (output1 -> Validation error field output2) -> ValidationResult error field output1 -> Validation error field output2
andThenVR f validationAction =
    case validationAction of
        VR_SUCCESS { output } ->
            f output

        VR_FAIL { errors } ->
            V_RESULT <| VR_FAIL { errors = errors }


andThen : (output1 -> Validation error field output2) -> Validation error field output1 -> Validation error field output2
andThen validationCont validation =
    case validation of
        V_ACTION va ->
            V_ACTION (andThenVA validationCont va)

        V_RESULT vr ->
            andThenVR validationCont vr


makeList : (Field.List x -> field) -> Validation error x output -> List UniqueIndex -> Validation error field (List output)
makeList fieldListF validation uidxs =
    case uidxs of
        [] ->
            V_RESULT <| VR_SUCCESS <| SuccessState []

        uidx :: rest ->
            let
                mappedValidation =
                    validation |> mapField (\x -> fieldListF <| Field.WithIndex uidx x)
            in
            succeed (::)
                |> andMap mappedValidation
                |> andMap (makeList fieldListF validation rest)

traverse : List (Validation error x output) -> Validation error x (List output)
traverse list = 
    case list of 
        [] -> 
            succeed [] 

        first :: rest -> 
            succeed (::)
                |> andMap first 
                |> andMap (traverse rest)


andMapVA : ValidationAction error field output1 -> Validation error field (output1 -> output2) -> ValidationAction error field output2
andMapVA validationAction validationF =
    mapVAVs (\v -> andMap v validationF) validationAction


andMapVR : ValidationResult error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMapVR validationResult validationF =
    case validationF of
        V_ACTION va ->
            V_ACTION <| mapVAVs (andMapVR validationResult) va

        V_RESULT vrf ->
            V_RESULT <|
                case vrf of
                    VR_FAIL failState2 ->
                        case validationResult of
                            VR_FAIL failState1 ->
                                VR_FAIL { errors = failState1.errors |> merge failState2.errors }

                            VR_SUCCESS _ ->
                                VR_FAIL failState2

                    VR_SUCCESS successState2 ->
                        case validationResult of
                            VR_FAIL failState1 ->
                                VR_FAIL failState1

                            VR_SUCCESS successState1 ->
                                VR_SUCCESS { output = successState2.output successState1.output }


andMap : Validation error field output1 -> Validation error field (output1 -> output2) -> Validation error field output2
andMap validation validationF =
    case validation of
        V_ACTION va ->
            V_ACTION <| andMapVA va validationF

        V_RESULT vr ->
            andMapVR vr validationF


andMapDiscard : Validation error field output1 -> Validation error field output2 -> Validation error field output2
andMapDiscard validation1 validation2 =
    succeed (\_ x -> x)
        |> andMap validation1
        |> andMap validation2


succeed : output -> Validation error field output
succeed output =
    V_RESULT <| VR_SUCCESS <| SuccessState output


failure : (Field.Value a -> field) -> error -> Validation error field output
failure fieldF error =
    V_RESULT <| VR_FAIL <| FailState <| Map.singleton (fieldF Field.Value) (Error error)

loading : (Field.Value a -> field) -> Validation error field output 
loading fieldF = 
    V_RESULT <| VR_FAIL <| FailState <| Map.singleton (fieldF Field.Value) Loading 


lazy : (() -> Validation error field output) -> Validation error field output
lazy =
    V_ACTION << VA_LAZY


resolve : Form error field output submitted -> Validation error field output -> ValidationResult error field output
resolve form validation =
    case validation of
        V_ACTION va ->
            case va of
                VA_STR field_ cont ->
                    let
                        stringValue =
                            Get.getString (Get.field (\_ -> field_)) form
                    in
                    resolve form <| cont stringValue

                VA_BOOL field_ cont ->
                    let
                        boolValue =
                            Get.getBool (Get.field (\_ -> field_)) form
                    in
                    resolve form <| cont boolValue

                VA_LIST fl contI ->
                    let
                        uniqueIndexes =
                            form |> Get.indexes (\_ -> fl)

                        -- we are sure its the correct index cause of fromList function
                    in
                    resolve form <| contI uniqueIndexes

                VA_LAZY f ->
                    resolve form (f ())

        V_RESULT vr ->
            vr


validate_ : Form error field output submitted -> Form error field output submitted
validate_ form =
    let
        validationResult =
            resolve form form.validation

        clearedForm =
            form |> clearErrors
    in
    case validationResult of
        VR_SUCCESS { output } ->
            { clearedForm | output = Just output }

        VR_FAIL { errors } ->
            { clearedForm | output = Nothing } |> setErrors (errors |> Map.toList)


clearErrors : Form error field output submitted -> Form error field output submitted
clearErrors form =
    { form | values = form.values |> FieldIndexDict.map (\state -> { state | errorState = FieldState.NoError }) }


setErrors : List ( field, FailCell error ) -> Form error field output submitted -> Form error field output submitted
setErrors errors form =
    case errors of
        [] ->
            form

        ( field_, failCell ) :: rest ->
            let
                ( fieldIndex, newForm ) =
                    case form.fieldIndexes |> Map.get field_ of
                        Nothing ->
                            ( form.fieldIndexToUse
                            , { form
                                | fieldIndexes = form.fieldIndexes |> Map.set field_ form.fieldIndexToUse
                                , fieldIndexToUse = form.fieldIndexToUse |> FieldIndex.next
                              }
                            )

                        Just fi ->
                            ( fi, form )

                errorState = 
                    case failCell of 
                        Error error -> 
                            FieldState.Error error 

                        Loading -> 
                            FieldState.Loading

                newValues =
                    case newForm.values |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            newForm.values |> FieldIndexDict.set fieldIndex { value = Nothing, errorState = errorState }

                        Just state ->
                            newForm.values |> FieldIndexDict.set fieldIndex { state | errorState = errorState }
            in
            setErrors rest { newForm | values = newValues }


merge : Map field (FailCell error) -> Map field (FailCell error) -> Map field (FailCell error)
merge fields1 fields2 =
    fields1
        |> Map.foldl
            (\field_ failCell m ->
                m
                    |> Map.updateWithDefault field_
                        (\mfailCell2 ->
                            case mfailCell2 of
                                Nothing ->
                                    failCell

                                Just failCell2 ->
                                    case failCell of 
                                        Loading -> 
                                            failCell2 

                                        Error error -> 
                                            failCell
                        )
            )
            fields2


mapFailState : (field1 -> field2) -> FailState error field1 -> FailState error field2
mapFailState mapf failState =
    { errors = failState.errors |> Map.mapBoth (\key failCell -> ( key |> mapf, failCell )) }


mapError : (error1 -> error2) -> Validation error1 field output -> Validation error2 field output
mapError f validation =
    case validation of
        V_ACTION va ->
            V_ACTION <| mapVAVs (mapError f) va

        V_RESULT vr ->
            V_RESULT <|
                case vr of
                    VR_SUCCESS { output } ->
                        VR_SUCCESS { output = output }

                    VR_FAIL { errors } ->
                        VR_FAIL { errors = errors |> Map.mapValue (\failCell -> case failCell of 
                            Error e -> 
                                Error (f e)
                            Loading -> 
                                Loading 
                        ) }
