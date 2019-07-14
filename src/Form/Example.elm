module Form.Example exposing (..)

import Form exposing (Field, FieldList, FieldNested, Form)
import Form.Transaction
import Form.Validation exposing (..)
import Form.View
import Html exposing (Html, button, div, input, p, span, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Index.UniqueIndex exposing (UniqueIndex)


-- FIELDS CAN BE EITHER STRING OR BOOL SINCE FIELDS IN HTML ARE ONLY THOSE
-- SETTING UP THE FIELDS OF THE FORM


type Msg
    = SetString (Field String -> MainField) String
    | SetInList UniqueIndex (FieldList OfferField1 -> MainField) (Field String -> OfferField1) String
    | AddRow (FieldList OfferField1 -> MainField)
    | RemoveRow UniqueIndex (FieldList OfferField1 -> MainField)
    | MainFormMsg (Form.View.FormMsg MainField)


type FormMsg a msg
    = T (Form.Transaction a)
    | NT msg


type alias Model =
    { form : Form }


type MainField
    = Field1 (Field String) -- SIMPLE STRING FIELD
    | OfferField (FieldNested OfferField1) -- NESTED FIELD
      -- | Field3 (FieldList (Types.Field String)) -- SIMPLE STRING LIST FIELD
    | NestedOffers (FieldList OfferField1) -- NESTED LIST FIELD



-- OfferField : FieldNested OfferField1 -> MainField
-- Name : Field String -> OfferField1
-- form |> at (Field1)
-- form |> at (OfferField <| nested Name)
-- SETTING UP NESTED FIELDS


type OfferField1
    = Name (Field String)
    | Price (Field String)



-- OUTPUT OF THE FORM


type alias Output =
    { field1 : Int
    , offer : Offer

    -- , field3 : List String
    , field4 : List Offer
    }



-- OUTPUT OF THE NESTED FORM


type alias Offer =
    { name : String
    , price : Maybe Int
    }


type alias Form =
    Form.Form () MainField Output



-- OFFER VALIDATION


offerValidation : Form.Validation () OfferField1 Offer
offerValidation =
    succeed Offer
        |> andMap (fromString Name string)
        |> andMap (fromString Price (optional int))



-- MAIN VALIDATION


validation : Form.Validation () MainField Output
validation =
    succeed Output
        |> andMap (fromString Field1 int)
        |> andMap (fromNested OfferField offerValidation)
        -- |> andMap (fromListString Field3 string)
        |> andMap (fromList NestedOffers offerValidation)



-- INIT AND SETTING UP INITIAL VALUES


initialOfferTransaction : Form.Transaction OfferField1
initialOfferTransaction =
    Form.Transaction.setString Name "name"


initialTransaction : Form.Transaction MainField
initialTransaction =
    Form.Transaction.batch
        [ Form.Transaction.setString Field1 "1"
        , Form.Transaction.setNested OfferField initialOfferTransaction
        , Form.Transaction.addRow NestedOffers
            initialOfferTransaction
        , Form.Transaction.addRow NestedOffers
            (Form.Transaction.batch
                [ Form.Transaction.setString Name "name2" ]
            )
        ]


init : Form
init =
    Form.form validation
        |> Form.Transaction.save initialTransaction
        |> (\form ->
                let
                    _ =
                        form |> Form.get Field1 |> Debug.log "field1"

                    _ =
                        form |> Form.get (OfferField |> Form.at Name) |> Debug.log "offerfield"

                    _ =
                        form |> Form.indexes NestedOffers |> Debug.log "nested offers"

                    _ =
                        form |> Form.indexes NestedOffers |> List.map (\uiq -> form |> Form.get (NestedOffers |> Form.atIndex uiq Name)) |> Debug.log "nested list"
                in
                form
           )


textInput : (Field String -> MainField) -> Form -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
textInput function form attributeHtmlList msgHtmlList =
    input ([ onInput (\s -> SetString function s), value (form |> Form.get function), type_ "text" ] ++ attributeHtmlList) msgHtmlList


view : Model -> Html Msg
view model =
    div []
        [ textInput Field1 model.form [] []
        , case model.form |> Form.getError Field1 of
            Nothing ->
                text ""

            Just error ->
                text "there is an error here"
        , p [] []
        , button [ onClick (AddRow NestedOffers) ] [ text "+" ]
        , model.form |> Form.indexes NestedOffers |> List.map (\uid -> viewOffer2 |> Form.View.inIndex uid NestedOffers) |> Form.View.div [] |> Form.View.inForm model.form |> Html.map MainFormMsg
        ]


viewOffer : UniqueIndex -> Model -> Html Msg
viewOffer uniqueIndex model =
    let
        _ =
            Debug.log "nestedoffers" NestedOffers
    in
    div []
        [ input [ onInput (SetInList uniqueIndex NestedOffers Name), value (model.form |> Form.get (NestedOffers |> Form.atIndex uniqueIndex Name)), type_ "text" ] []
        , input [ onInput (SetInList uniqueIndex NestedOffers Price), value (model.form |> Form.get (NestedOffers |> Form.atIndex uniqueIndex Price)), type_ "text" ] []
        , button [ onClick (RemoveRow uniqueIndex NestedOffers) ] [ text "-" ]
        ]


viewOffer2 : Form.View () OfferField1 (Form.View.FormMsg OfferField1)
viewOffer2 =
    Form.View.div
        []
        [ textInput2 Name
        , textInput2 Price
        ]


textInput2 : (Field String -> field) -> Form.View () field (Form.View.FormMsg field)
textInput2 fieldF =
    Form.View.stringInput fieldF
        (\onInputMsg str error ->
            span
                []
                [ input [ onInput onInputMsg, value str ] []
                , case error of
                    Nothing ->
                        text ""

                    Just _ ->
                        text " Error"
                ]
        )



-- viewOffer : List (FormHtml )
-- view : Model -> Html.Html Msg
-- view model =
--     Html.text ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetString function string ->
            { model | form = model.form |> Form.Transaction.save (Form.Transaction.setString function string) }

        AddRow function ->
            { model | form = model.form |> Form.Transaction.save (Form.Transaction.addRow function Form.Transaction.empty) }

        RemoveRow uniqueIndex function ->
            { model | form = model.form |> Form.Transaction.save (Form.Transaction.removeRow function uniqueIndex) }

        SetInList uniqueIndex listF nestedF string ->
            { model | form = model.form |> Form.Transaction.save (Form.Transaction.setAtIndex listF uniqueIndex (Form.Transaction.setString nestedF string)) }

        MainFormMsg formMsg ->
            { model | form = Form.View.update formMsg model.form }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = { form = init }
        , view = view
        , update = update
        }
