module Form.View.Html exposing (..)

import Form.View2
import Html exposing (Html, Attribute)
import Html.Attributes
import Form.Transaction exposing (Transaction)
import Form.FieldState as FieldState
import Form.Get as Get
import Form.Field as Field


type View error field msg 
    = VH_NODE String (List (Attribute msg)) (List (View error field msg))
    | VH_INTERNAL (Form.View2.View error field (Html msg)) 

node : String -> List (Attribute msg) -> List (View error field msg) -> View error field msg
node =
    VH_NODE

div : List (Html.Attribute msg) -> List (View error field msg) -> View error field msg
div = node "div"

stringInput : (Field.Value String -> field) -> ((String -> Transaction field) -> Get.ValueState String -> FieldState.ErrorState error -> Html msg) -> View error field msg
stringInput field f =
    Form.View2.stringInput field f |> VH_INTERNAL


inForm : Form.View2.Form error field output -> View error field msg -> Html msg 
inForm form view = 
    case view of 
        VH_NODE nodeName attributes views -> 
            Html.node nodeName attributes (views |> List.map (inForm form))

        VH_INTERNAL internalView -> 
            Form.View2.inForm form internalView
