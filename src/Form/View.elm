module Form.View exposing (..)

import Form exposing (View(..))
import Form.Transaction
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Index.UniqueIndex exposing (UniqueIndex)


nested : (Form.FieldNested field1 -> field2) -> View error field1 (FormMsg field1) -> View error field2 (FormMsg field2)
nested fieldF view =
    case view of
        VI_STRING field f ->
            VI_STRING (Form.fieldNestedNotOpaque fieldF << field) (\s me -> f s me |> Html.map (mapMsg <| Form.fieldNestedNotOpaque fieldF))

        VI_VIEW nodeName attrs views ->
            VI_VIEW nodeName (attrs |> List.map (Html.Attributes.map (mapMsg <| Form.fieldNestedNotOpaque fieldF))) (views |> List.map (nested fieldF))

        VI_HTML html ->
            VI_HTML (html |> Html.map (mapMsg <| Form.fieldNestedNotOpaque fieldF))

        VI_REMOVELASTROW field f ->
            VI_REMOVELASTROW (Form.fieldNestedNotOpaque fieldF field) (\uiq -> f uiq |> Html.map (mapMsg <| Form.fieldNestedNotOpaque fieldF))

        VI_INLIST field f ->
            VI_INLIST (Form.fieldNestedNotOpaque fieldF field) (\uiqs -> f uiqs |> nested fieldF)

        VI_LAZY f ->
            VI_LAZY (f >> nested fieldF)


inIndex : UniqueIndex -> (Form.FieldList field1 -> field2) -> View error field1 (FormMsg field1) -> View error field2 (FormMsg field2)
inIndex uniqueIndex fieldF view =
    case view of
        VI_STRING field f ->
            VI_STRING (Form.listField fieldF uniqueIndex << field) (\s me -> f s me |> Html.map (mapMsg <| Form.listField fieldF uniqueIndex))

        VI_VIEW nodeName attrs views ->
            VI_VIEW nodeName (attrs |> List.map (Html.Attributes.map (mapMsg <| Form.listField fieldF uniqueIndex))) (views |> List.map (inIndex uniqueIndex fieldF))

        VI_HTML html ->
            VI_HTML (html |> Html.map (mapMsg <| Form.listField fieldF uniqueIndex))

        VI_REMOVELASTROW field f ->
            VI_REMOVELASTROW (Form.listField fieldF uniqueIndex field) (\uiq -> f uiq |> Html.map (mapMsg <| Form.listField fieldF uniqueIndex))

        VI_INLIST field f ->
            VI_INLIST (Form.listField fieldF uniqueIndex field) (\uiqs -> f uiqs |> inIndex uniqueIndex fieldF)

        VI_LAZY f ->
            VI_LAZY (f >> inIndex uniqueIndex fieldF)


inForm : Form.Form error field output -> View error field msg -> Html msg
inForm form view =
    case view of
        VI_STRING field f ->
            f (form |> Form.get field) (form |> Form.getError field)

        VI_VIEW nodeName attrs views ->
            Html.node nodeName attrs (views |> List.map (inForm form))

        VI_HTML html ->
            html

        VI_REMOVELASTROW field f ->
            f (form |> Form.indexes (\_ -> field) |> List.reverse |> List.head)

        VI_INLIST field f ->
            f (form |> Form.indexes (\_ -> field)) |> inForm form

        VI_LAZY f ->
            inForm form (f ())


type FormMsg field
    = FormMsg (Form.Transaction field)


mapMsg : (field1 -> field2) -> FormMsg field1 -> FormMsg field2
mapMsg f (FormMsg t) =
    FormMsg (Form.Transaction.map f t)


stringInput : (Form.Field String -> field) -> ((String -> FormMsg field) -> String -> Maybe error -> Html msg) -> View error field msg
stringInput field f =
    VI_STRING field (f (Form.Transaction.setString field >> FormMsg))


node : String -> List (Attribute msg) -> List (View error field msg) -> View error field msg
node nodeName attrs views =
    VI_VIEW nodeName attrs views


div : List (Attribute msg) -> List (View error field msg) -> View error field msg
div =
    node "div"


addRow : (Form.FieldList field2 -> field1) -> (FormMsg field1 -> Html msg) -> View error field1 msg
addRow fieldListF viewF =
    VI_HTML (viewF (FormMsg (Form.Transaction.addRow fieldListF Form.Transaction.empty)))


removeRow : (Form.FieldList field2 -> field1) -> UniqueIndex -> (FormMsg field1 -> Html msg) -> View error field1 msg
removeRow fieldListF uniqueIndex viewF =
    VI_HTML (viewF (FormMsg (Form.Transaction.removeRow fieldListF uniqueIndex)))


removeLastRow : (Form.FieldList field2 -> field1) -> (Maybe (FormMsg field1) -> Html msg) -> View error field1 msg
removeLastRow fieldListF f =
    VI_REMOVELASTROW (Form.listOpaque fieldListF) (\muid -> f (muid |> Maybe.map (\uid -> FormMsg (Form.Transaction.removeRow fieldListF uid))))


inList : (Form.FieldList field2 -> field1) -> (List UniqueIndex -> View error field1 msg) -> View error field1 msg
inList fieldListF f =
    VI_INLIST (Form.listOpaque fieldListF) f


update : FormMsg field -> Form.Form error field output -> Form.Form error field output
update msg form =
    case msg of
        FormMsg transaction ->
            form |> Form.Transaction.save transaction
