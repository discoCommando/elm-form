module Form.Validation exposing
    ( Form
    , Validation
    , andMap
    , andThen
    , failure
    , fromList
    , fromNested
    , fromString
    , int
    , lazy
    , makeList
    , map
    , mapField
    , optional
    , anyString
    , nonEmptyString
    , succeed
    , validate_
    , andMapDiscard
    , mapError
    , required
    )

import Form.Field as Field
import Form.FieldState
import Form.Get as Get
import Form.Map as Map exposing (Map)
import Form.Type
import Index.FieldIndex as FieldIndex
import Index.FieldIndexDict as FieldIndexDict
import Index.UniqueIndex exposing (UniqueIndex)
import Form.CommonError exposing (CommonError(..))


type Validation error field output
    = V_ACTION (ValidationAction error field output)
    | V_RESULT (ValidationResult error field output)


type ValidationAction error field output
    = VA_STR field (Get.Result String -> Validation error field output)
    | VA_LIST field (List UniqueIndex -> Validation error field output)
    | VA_LAZY (() -> Validation error field output)

type alias FailCell error =
    { error : Maybe error }


type alias FailState error field =
    { errors : Map field (FailCell error) }


type alias SuccessState output =
    { output : output }


type ValidationResult error field output
    = VR_FAIL (FailState error field)
    | VR_SUCCESS (SuccessState output)


type alias Form error field output =
    Form.Type.Form error field output (Validation error field output)


anyString : String -> Result x String
anyString s =
    Ok s

nonEmptyString : String -> Result CommonError String 
nonEmptyString s =
    case s of 
        "" -> 
            Err NotFound 

        _ -> 
            Ok s 


int : String -> Result () Int
int =
    String.toInt >> Result.fromMaybe ()


optional : (a -> Result error output) -> (Get.Result a -> Result error (Maybe output))
optional f gr =
    case gr of
        Get.NotEdited ->
            Ok Nothing

        Get.Edited s ->
            f s |> Result.map Just

required : Bool -> (a -> Result CommonError output) -> (Get.Result a -> Result CommonError output)
required submitted f gr = 
    case gr of 
        Get.NotEdited -> 
            if submitted then 
                Err NotFound
            else 
                Err NotEditedYet 

        Get.Edited a -> 
            f a


fromString : (Field.Value String -> field) -> (Get.Result String -> Result error output) -> Validation error field output
fromString fieldF strVal =
    V_ACTION <|
        VA_STR
            (fieldF Field.Value)
            (\s ->
                case strVal s of
                    Err error ->
                        failure fieldF error

                    Ok v ->
                        succeed v
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

        VA_LIST fl fi ->
            VA_LIST fl (fi >> f)

        VA_LAZY f_ ->
            VA_LAZY (f_ >> f)


mapVA : (o1 -> o2) -> ValidationAction error field o1 -> ValidationAction error field o2
mapVA f =
    mapVAVs (map f)


mapVR : (o1 -> o2) -> ValidationResult error field o1 -> ValidationResult error field o2
mapVR f validationResult =
    case validationResult of
        VR_SUCCESS { output } ->
            VR_SUCCESS { output = f output }

        VR_FAIL failState ->
            VR_FAIL failState


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
    V_RESULT <| VR_FAIL <| FailState <| Map.singleton (fieldF Field.Value) (FailCell (Just error))


lazy : (() -> Validation error field output) -> Validation error field output
lazy =
    V_ACTION << VA_LAZY


resolve : Form error field output -> Validation error field output -> ValidationResult error field output
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


validate_ : Form error field output -> Form error field output
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


clearErrors : Form error field output -> Form error field output
clearErrors form =
    { form | values = form.values |> FieldIndexDict.map (\state -> { state | error = Nothing }) }


setErrors : List ( field, FailCell error ) -> Form error field output -> Form error field output
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

                newValues =
                    case newForm.values |> FieldIndexDict.get fieldIndex of
                        Nothing ->
                            newForm.values |> FieldIndexDict.set fieldIndex { value = Form.FieldState.FVEmpty, error = failCell.error }

                        Just state ->
                            newForm.values |> FieldIndexDict.set fieldIndex { state | error = failCell.error }
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
                                    case failCell.error of
                                        Nothing ->
                                            failCell2

                                        _ ->
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
            V_RESULT <| case vr of  
                VR_SUCCESS { output } ->
                    VR_SUCCESS { output = output }

                VR_FAIL { errors } ->
                    VR_FAIL { errors = errors |> Map.mapValue (\{ error } -> { error = Maybe.map f error } )}
