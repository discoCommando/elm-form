module Form.View exposing
    ( Form
    , FormMsg(..)
    , Submitted(..)
    , View
    , addRow
    , boolInput
    , div
    , inForm
    , inIndex
    , inList
    , mapMsg
    , nested
    , node
    , removeLastRow
    , removeRow
    , stringInput
    , update
    , submit
    )

import Form.Field as Field
import Form.Get as Get
import Form.Transaction exposing (Transaction)
import Form.Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Index.UniqueIndex exposing (UniqueIndex)
import Form.FieldState as FieldState


type View error field msg
    = VI_STRING (Field.Value String -> field) (Get.ValueState String -> FieldState.ErrorState error -> Html msg)
    | VI_BOOL (Field.Value Bool -> field) (Get.ValueState Bool -> FieldState.ErrorState error -> Html msg)
    | VI_HTML (Html msg) 
    | VI_VIEW String (List (Html.Attribute msg)) (List (View error field msg))
    | VI_REMOVELASTROW field (Maybe UniqueIndex -> Html msg)
    | VI_INLIST field (List UniqueIndex -> View error field msg)
    | VI_LAZY (() -> View error field msg)


type Submitted
    = Submitted
    | NotSubmitted


type alias Form error field output =
    Form.Validation.Form error field output Submitted


nested : (Field.Nested field1 -> field2) -> View error field1 (FormMsg field1) -> View error field2 (FormMsg field2)
nested fieldF view =
    case view of
        VI_STRING field f ->
            VI_STRING (fieldF << Field.WithValue << field) (\s me -> f s me |> Html.map (mapMsg <| (fieldF << Field.WithValue)))

        VI_BOOL field f ->
            VI_BOOL (fieldF << Field.WithValue << field) (\s me -> f s me |> Html.map (mapMsg <| (fieldF << Field.WithValue)))

        VI_VIEW nodeName attrs views ->
            VI_VIEW nodeName (attrs |> List.map (Html.Attributes.map (mapMsg <| (fieldF << Field.WithValue)))) (views |> List.map (nested fieldF))

        VI_HTML html ->
            VI_HTML (html |> Html.map (mapMsg <| (fieldF << Field.WithValue)))

        VI_REMOVELASTROW field f ->
            VI_REMOVELASTROW (fieldF <| Field.WithValue field) (\uiq -> f uiq |> Html.map (mapMsg <| (fieldF << Field.WithValue)))

        VI_INLIST field f ->
            VI_INLIST (fieldF <| Field.WithValue field) (\uiqs -> f uiqs |> nested fieldF)

        VI_LAZY f ->
            VI_LAZY (f >> nested fieldF)


inIndex : UniqueIndex -> (Field.List field1 -> field2) -> View error field1 (FormMsg field1) -> View error field2 (FormMsg field2)
inIndex uniqueIndex fieldF view =
    case view of
        VI_STRING field f ->
            VI_STRING (fieldF << Field.WithIndex uniqueIndex << field) (\s me -> f s me |> Html.map (mapMsg (fieldF << Field.WithIndex uniqueIndex)))

        VI_BOOL field f ->
            VI_BOOL (fieldF << Field.WithIndex uniqueIndex << field) (\s me -> f s me |> Html.map (mapMsg (fieldF << Field.WithIndex uniqueIndex)))

        VI_VIEW nodeName attrs views ->
            VI_VIEW nodeName (attrs |> List.map (Html.Attributes.map (mapMsg (fieldF << Field.WithIndex uniqueIndex)))) (views |> List.map (inIndex uniqueIndex fieldF))

        VI_HTML html ->
            VI_HTML (html |> Html.map (mapMsg (fieldF << Field.WithIndex uniqueIndex)))

        VI_REMOVELASTROW field f ->
            VI_REMOVELASTROW (fieldF <| Field.WithIndex uniqueIndex field) (\uiq -> f uiq |> Html.map (mapMsg (fieldF << Field.WithIndex uniqueIndex)))

        VI_INLIST field f ->
            VI_INLIST (fieldF <| Field.WithIndex uniqueIndex field) (\uiqs -> f uiqs |> inIndex uniqueIndex fieldF)

        VI_LAZY f ->
            VI_LAZY (f >> inIndex uniqueIndex fieldF)


inForm : Form error field output -> View error field msg -> Html msg
inForm form view =
    case view of
        VI_STRING field f ->
            f (form |> Get.getString (Get.field field)) (form |> Get.getError (Get.field field))

        VI_BOOL field f ->
            f (form |> Get.getBool (Get.field field)) (form |> Get.getError (Get.field field))

        VI_VIEW nodeName attrs views ->
            Html.node nodeName attrs (views |> List.map (inForm form))

        VI_HTML html ->
            html

        VI_REMOVELASTROW field f ->
            f (form |> Get.indexes (\_ -> field) |> List.reverse |> List.head)

        VI_INLIST field f ->
            f (form |> Get.indexes (\_ -> field)) |> inForm form

        VI_LAZY f ->
            inForm form (f ())


type FormMsg field
    = TransactionMsg (Transaction field)
    | SubmitMsg 


mapMsg : (field1 -> field2) -> FormMsg field1 -> FormMsg field2
mapMsg f msg =
    case msg of
        TransactionMsg t ->
            TransactionMsg (Form.Transaction.map f t)

        SubmitMsg -> 
            SubmitMsg 

   

stringInput : (Field.Value String -> field) -> ((String -> FormMsg field) -> Get.ValueState String -> FieldState.ErrorState error -> Html msg) -> View error field msg
stringInput field f =
    VI_STRING field (f (Form.Transaction.setString field >> TransactionMsg))


boolInput : (Field.Value Bool -> field) -> ((Bool -> FormMsg field) -> Get.ValueState Bool -> FieldState.ErrorState error -> Html msg) -> View error field msg
boolInput field f =
    VI_BOOL field (f (Form.Transaction.setBool field >> TransactionMsg))

node : String -> List (Attribute msg) -> List (View error field msg) -> View error field msg
node nodeName attrs views =
    VI_VIEW nodeName attrs views


div : List (Attribute msg) -> List (View error field msg) -> View error field msg
div =
    node "div"


addRow : (Field.List field2 -> field1) -> (FormMsg field1 -> Html msg) -> View error field1 msg
addRow fieldListF viewF =
    VI_HTML (viewF (TransactionMsg (Form.Transaction.addRow fieldListF Form.Transaction.empty)))


removeRow : (Field.List field2 -> field1) -> UniqueIndex -> (FormMsg field1 -> Html msg) -> View error field1 msg
removeRow fieldListF uniqueIndex viewF =
    VI_HTML (viewF (TransactionMsg (Form.Transaction.removeRow fieldListF uniqueIndex)))


removeLastRow : (Field.List field2 -> field1) -> (Maybe (FormMsg field1) -> Html msg) -> View error field1 msg
removeLastRow fieldListF f =
    VI_REMOVELASTROW (fieldListF Field.OpaqueList) (\muid -> f (muid |> Maybe.map (\uid -> TransactionMsg (Form.Transaction.removeRow fieldListF uid))))


inList : (Field.List field2 -> field1) -> (List UniqueIndex -> View error field1 msg) -> View error field1 msg
inList fieldListF f =
    VI_INLIST (fieldListF Field.OpaqueList) f


update : FormMsg field -> Form error field output -> Form error field output
update msg form =
    case msg of
        TransactionMsg transaction ->
            form |> Form.Transaction.save transaction

        SubmitMsg -> 
            { form | submitted = Submitted}

isSubmitMsg : FormMsg field -> Bool 
isSubmitMsg msg = 
    case msg of 
        SubmitMsg -> 
            True 

        _ -> 
            False 

submit : (FormMsg field -> Html msg) -> View error field msg 
submit submitFunction = 
    VI_HTML (submitFunction SubmitMsg)