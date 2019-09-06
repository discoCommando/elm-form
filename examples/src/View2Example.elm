module View2Example exposing (..)


import Form 
import Form.Validation exposing (..)
import Form.Field as Field 
import Form.Get as Get
import Form.CommonError exposing (CommonError)
import Html 
import Form.View.Element as Element
import Element
import Form.Transaction as Transaction 

type Field 
    = FirstName (Field.Value String)
    | Number (Field.Value String)
    | Password (Field.Value String)
    | RepeatPassword (Field.Value String)

type alias Output = { firstName: String, number: Maybe Int, password : Password }

type alias Form =
    Form.Form CommonError Field Result 


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


form : Form 
form = Form.form validation 


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

type Element msg = Element.Element CommonError Field msg

viewForm : Element (Transaction Field)
viewForm = 
    Element.column
        [ Element_]
        [ 
        ] 
     

