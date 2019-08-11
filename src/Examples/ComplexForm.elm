module ComplexForm exposing (..)

import Form
import Form.CommonError as CommonError exposing (CommonError(..))
import Form.Field as Field
import Form.Validation
    exposing
        ( andMap
        , andMapDiscard
        , andThen
        , anyString
        , failure
        , fromBool
        , fromString
        , isTrue
        , map
        , mapError
        , nonEmptyString
        , optional
        , required
        , succeed
        )
import Form.View exposing (Submitted(..))
import Form.Get as Get
import Html exposing (Html, text, span, input, button, p, div)
import Html.Attributes exposing (type_, selected, checked, value)
import Html.Events exposing (onInput, onCheck, onClick)
import Browser

type Plan
    = Basic
    | Pro
    | Enterprise


type Email
    = Email_ String


type Password
    = Password_ String


type Field
    = Name (Field.Value String)
    | Email (Field.Value String)
    | Password (Field.Value String)
    | RepeatPassword (Field.Value String)
    | Plan (Field.Value String)
    | AgreedToTerms (Field.Value Bool)


type alias UserDetails =
    { name : Maybe String
    , email : Email
    , password : Password
    , plan : Plan
    }


type alias Form =
    Form.Form CommonError Field UserDetails


type alias Validation output =
    Form.Validation CommonError Field output


parseEmail : String -> Result CommonError Email
parseEmail s =
    if String.contains "@" s then
        Ok <| Email_ s

    else
        Err <| CommonError.custom "Invalid email"


parsePassword s =
    if String.length s >= 6 then
        Ok <| Password_ s

    else
        Err <| CommonError.custom "Password must be at least 6 characters"


parsePlan s =
    case s of
        "Basic" ->
            Ok Basic

        "Pro" ->
            Ok Pro

        "Enterprise" ->
            Ok Enterprise

        _ ->
            Err <| CommonError.custom "Invalid plan"


passwordValidation : Validation Password
passwordValidation  =
    succeed (\a b -> ( a, b ))
        |> andMap (fromString Password <| required parsePassword)
        |> andMap (fromString RepeatPassword <| required anyString)
        |> andThen
            (\( Password_ p, rp ) ->
                if p == rp then
                    Password_ p |> succeed

                else
                    failure RepeatPassword <| CommonError.custom "The passwords must match"
            )


validation : Validation UserDetails
validation =
    succeed UserDetails
        |> andMap (fromString Name <| optional anyString)
        |> andMap (fromString Email <| required parseEmail)
        |> andMap passwordValidation
        |> andMapDiscard (isTrue AgreedToTerms)
        |> andMap (fromString Plan <| required parsePlan)
    

--view 
errorText : Get.Result a -> Maybe CommonError -> Submitted -> Html msg 
errorText gr me formSubmitted = 
    case me of 
        Nothing -> 
            text ""

        Just e -> 
            let 
                errorT = 
                    case e of 
                        CommonError.NoInput -> 
                            "Field is required"

                        CommonError.Custom s -> 
                            s 
            in
            case gr of 
                Get.NotEdited -> 
                    case formSubmitted of 
                        Form.View.Submitted -> 
                            text errorT

                        Form.View.NotSubmitted -> 
                            text "" 

                Get.Edited _ -> 
                    text errorT


textInput : (Field.Value String -> field) -> String -> Submitted -> Form.View CommonError field (Form.View.FormMsg field)
textInput fieldF label formSubmitted =
    Form.View.stringInput fieldF
        (\onInputMsg gStr error ->
            let 
                str = gStr |> Get.toMaybe |> Maybe.withDefault ""
            in
            p
                []
                [ text label, input [ onInput onInputMsg, value str ] []
                , errorText gStr error formSubmitted
                ]
        )

checkbox : (Field.Value Bool -> field) -> String -> Form.Submitted -> Form.View CommonError field (Form.View.FormMsg field)
checkbox fieldF label formSubmitted = 
    Form.View.boolInput fieldF
        (\onInputMsg gBool error ->
            let 
                bool = gBool |> Get.toMaybe |> Maybe.withDefault False
            in
            p
                []
                [ text label, input [ onCheck onInputMsg, type_ "checkbox", checked bool ] []
                , errorText gBool error formSubmitted
                ]
        )

select : (Field.Value String -> field) -> String -> List String -> Form.Submitted -> Form.View CommonError field (Form.View.FormMsg field)
select fieldF label values formSubmitted  = 
    Form.View.stringInput fieldF 
        (\onInputMsg gStr error ->
            let 
                str = gStr |> Get.toMaybe |> Maybe.withDefault ""

                entry v = 
                    Html.option [ value v, selected (str == v) ] [ text v ]

                entries = 
                    List.map entry values 

                options =
                    (Html.option [ ] [ text "Choose plan" ]) :: entries
            in
            p
                []
                [ text label, Html.select [ onInput onInputMsg ] options
                , errorText gStr error formSubmitted
                ]
        )

submitButton : Form.View error field (Form.View.FormMsg field)
submitButton = Form.View.submit (\msg -> 
    button [ onClick msg ] [ text "Submit" ]) 


formView : Form.Submitted -> Form.View CommonError Field (Form.View.FormMsg Field)
formView formSubmitted = 
    Form.View.div 
        [] 
        [ textInput Name "Name" formSubmitted
        , textInput Email "Email" formSubmitted
        , textInput Password "Password" formSubmitted
        , textInput RepeatPassword "Repeat password" formSubmitted
        , select Plan "Choose a plan" ["Basic", "Pro", "Enterprise", "Weird"] formSubmitted 
        , checkbox AgreedToTerms "I agree to terms and conditions" formSubmitted
        , submitButton
        ]

type alias Model = { form : Form }

view : Model -> Html Msg 
view { form } = 
    div [] [ 
        Form.View.inForm form (formView form.submitted) |> Html.map Msg
        , p [] [text <| Debug.toString form.output] ]

type Msg = 
    Msg (Form.View.FormMsg Field)

update : Msg -> Model -> Model 
update (Msg msg) { form } = 
    { form = form |> Form.View.update msg }

init : Form 
init = Form.form validation

main : Program () Model Msg
main =
    Browser.sandbox
        { init = { form = init }
        , view = view
        , update = update
        }


