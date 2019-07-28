module Form.Example2 exposing (Form, Model, Msg(..), Tree(..), TreeField(..), init, main, update, validation, view, viewTree)

import Browser
import Form
import Form.Validation
import Form.View
import Html exposing (button, input, p, text)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)


type TreeField
    = Value (Form.Field String)
    | Children (Form.FieldList TreeField)


type Tree
    = Tree Int (List Tree)


type Msg
    = Msg (Form.View.FormMsg TreeField)


type alias Form =
    Form.Form () TreeField Tree


type alias Model =
    Form


validation : Form.Validation () TreeField Tree
validation =
    Form.Validation.succeed Tree
        |> Form.Validation.andMap (Form.Validation.fromString Value Form.Validation.int)
        |> Form.Validation.andMap (Form.Validation.lazy (\_ -> Form.Validation.fromList Children validation))


viewTree : Form.View () TreeField (Form.View.FormMsg TreeField)
viewTree =
    Form.View.div []
        [ Form.View.stringInput Value
            (\msg s e ->
                p []
                    [ input [ type_ "text", value s, onInput msg ] []
                    , case e of
                        Nothing ->
                            text ""

                        Just _ ->
                            text " error "
                    ]
            )
        , Form.View.addRow Children (\msg -> button [ onClick msg ] [ text " + " ])
        , Form.View.removeLastRow Children
            (\mmsg ->
                case mmsg of
                    Nothing ->
                        button [ disabled True ] [ text " - " ]

                    Just msg ->
                        button [ onClick msg ] [ text " - " ]
            )
        , Form.View.inList Children (\uids -> Form.View.div [ style "padding-left" "20px" ] (uids |> List.map (\uid -> Form.View.inIndex uid Children viewTree)))
        ]


view : Model -> Html.Html Msg
view form =
    Form.View.inForm form viewTree |> Html.map Msg


update : Msg -> Model -> Model
update (Msg formMsg) =
    Form.View.update formMsg


init : Model
init =
    Form.form validation


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
