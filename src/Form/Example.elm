module Form.Example exposing (..)

import Form.Form
import Form.Transaction
import Form.Types
import Form.Validation exposing (..)
import Html


-- FIELDS CAN BE EITHER STRING OR BOOL SINCE FIELDS IN HTML ARE ONLY THOSE
-- SETTING UP THE FIELDS OF THE FORM


type Msg
    = None


type alias Model =
    { form : Form }


type Field
    = Field1 (Form.Types.Field String) -- SIMPLE STRING FIELD
    | OfferField (Form.Types.FieldNested OfferField1) -- NESTED FIELD
      -- | Field3 (Form.Types.FieldList (Form.Types.Field String)) -- SIMPLE STRING LIST FIELD
    | NestedOffers (Form.Types.FieldList (Form.Types.FieldNested OfferField1)) -- NESTED LIST FIELD



-- SETTING UP NESTED FIELDS


type OfferField1
    = Name (Form.Types.Field String)
    | Price (Form.Types.Field String)



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
    Form.Types.Form () Field Output



-- OFFER VALIDATION


offerValidation : Form.Types.Validation () OfferField1 Offer
offerValidation =
    succeed Offer
        |> andMap (fromString Name string)
        |> andMap (fromString Price (int >> optional))



-- MAIN VALIDATION


validation : Form.Types.Validation () Field Output
validation =
    succeed Output
        |> andMap (fromString Field1 int)
        |> andMap (fromNested OfferField offerValidation)
        -- |> andMap (fromListString Field3 string)
        |> andMap (fromList NestedOffers offerValidation)



-- INIT AND SETTING UP INITIAL VALUES


initialOfferTransaction : Form.Transaction.Transaction OfferField1
initialOfferTransaction =
    Form.Transaction.setString Name "name"


initialTransaction : Form.Transaction.Transaction Field
initialTransaction =
    Form.Transaction.batch
        [ Form.Transaction.setString Field1 "1"
        , Form.Transaction.setNested OfferField initialOfferTransaction
        , Form.Transaction.addRow NestedOffers
            initialOfferTransaction
        , Form.Transaction.addRow NestedOffers
            (Form.Transaction.batch
                []
            )
        ]


init : Form
init =
    Form.Form.form validation
        |> Form.Transaction.save initialTransaction



-- view : Form -> Html FormMsg
-- view form =
--     Form.view form
--         [ textInput Field1 []
--         ,
--         ]
-- viewOffer : List (FormHtml )


view : Model -> Html.Html Msg
view model =
    Html.text ""


update : Msg -> Model -> Model
update msg model =
    model


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = { form = init }
        , view = view
        , update = update
        }
