module View2Example exposing (..)


import Form 
import Form.Validation exposing (succeed, andMap, string, fromString, required, failure, andThen, int, optional)
import Form.FieldState as FieldState exposing (ErrorState(..))
import Form.Field as Field 
import Form.Get as Get
import Form.CommonError as CommonError exposing (CommonError(..))
import Html 
import Form.View.Element as Element
import Form.View.Element.Input as Input
import Element as Element_
import Element.Font as Font 
import Form.Transaction as Transaction  exposing (Transaction)
import Html exposing (Html)
import Browser

type Field 
    = FirstName (Field.Value String)
    | Number (Field.Value String)
    | Password (Field.Value String)
    | RepeatPassword (Field.Value String)

type alias Output = { firstName: String, number: Maybe Int, password : Password }

type alias Form =
    Form.Form CommonError Field Output 


type alias Validation output =
    Form.Validation CommonError Field output

type Password
    = Password_ String

parsePassword s =
    if String.length s >= 6 then
        Ok <| Password_ s

    else
        Err <| CommonError.custom "Password must be at least 6 characters"



passwordValidation : Validation Password
passwordValidation =
    succeed (\a b -> ( a, b ))
        |> andMap (fromString Password <| required parsePassword)
        |> andMap (fromString RepeatPassword <| required string)
        |> andThen
            (\( Password_ p, rp ) ->
                if p == rp then
                    Password_ p |> succeed

                else
                    failure RepeatPassword <| CommonError.custom "The passwords must match"
            )

validation : Validation Output 
validation = 
    succeed Output 
        |> andMap (fromString FirstName <| required string)
        |> andMap (fromString Number <| optional int)
        |> andMap passwordValidation


initialForm : Form 
initialForm = Form.form validation 


type alias Model = { form : Form }

type Msg = T (Transaction Field)

update : Msg -> Model -> Model 
update msg model =
    case msg of 
        T transaction -> 
            { form = model.form |> Transaction.save transaction }

view : Model -> Html Msg 
view { form } = 
    viewForm |> Element.inForm form |> Element_.layout [] |> Html.map T

type alias Element msg = Element.Element CommonError Field msg
type alias Attribute msg = Element.Attribute CommonError Field msg 

viewForm : Element (Transaction Field)
viewForm = 
    Element.column
        [ Element_.spacing 10 |> Element.pureAttribute ]
        [ Input.text FirstName (\error -> 
            { attributes = errorAttributes error
            , label = Input.labelLeft [ Element.centerX, Element.centerY, Element_.width (Element_.px 100) |> Element.pureAttribute ] (Element_.text "First name" |> Element.pureElement)
            , placeholder = Just <| Input.placeholder [ Element.centerX, Element.centerY ] (Element_.text "Adam etc..." |> Element.pureElement)
            })
        , Input.text Number (\error -> 
            { attributes = errorAttributes error  
            , label = Input.labelLeft [ Element.centerX, Element.centerY, Element_.width (Element_.px 100) |> Element.pureAttribute ] (Element_.text "Number" |> Element.pureElement)
            , placeholder = Just <| Input.placeholder [ Element.centerX, Element.centerY ] (Element_.text "1,2.." |> Element.pureElement)
            })
        , Input.newPassword Password (\error -> 
            { attributes = errorAttributes error  
            , label = Input.labelLeft [ Element.centerX, Element.centerY, Element_.width (Element_.px 100) |> Element.pureAttribute ] (Element_.text "Password" |> Element.pureElement)
            , placeholder = Just <| Input.placeholder [ Element.centerX, Element.centerY ] (Element_.text "abc123.." |> Element.pureElement)
            , show = False 
            })
        , Input.newPassword RepeatPassword (\error -> 
            { attributes = errorAttributes error 
            , label = Input.labelLeft [ Element.centerX, Element.centerY, Element_.width (Element_.px 100) |> Element.pureAttribute ] (Element_.text "Password" |> Element.pureElement)
            , placeholder = Just <| Input.placeholder [ Element.centerX, Element.centerY ] (Element_.text "abc123.." |> Element.pureElement)
            , show = True
            })
        ] 
     
errorAttributes : FieldState.ErrorState CommonError -> List (Attribute (Transaction Field))
errorAttributes error =
    case error of 
        NoError -> [] 
        Loading -> []
        Error e -> 
            let 
                t = 
                    case e of 
                        NoInput -> 
                            "Field is required"

                        Custom s -> 
                            s 
            in 
            [ Element.onRight <| Element.el [Font.color (Element_.rgb 1 0 0) |> Element.pureAttribute, Element.centerX, Element.centerY ] <| Element.pureElement <| Element_.text t ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { form = initialForm }
        , view = view
        , update = update
        }