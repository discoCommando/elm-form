module Form.Example exposing (..)

import Form.Fields
import Form.Form
import Form.Types
import Form.Validation exposing (..)


-- FIELDS CAN BE EITHER STRING OR BOOL SINCE FIELDS IN HTML ARE ONLY THOSE 

-- SETTING UP THE FIELDS OF THE FORM

type Field
    = Field1 (Form.Types.Field String) -- SIMPLE STRING FIELD
    | OfferField OfferField1 -- NESTED FIELD 
    | Field3 Int (Form.Types.Field String) -- SIMPLE STRING LIST FIELD
    | NestedOffers Int OfferField1 -- NESTED LIST FIELD 

-- SETTING UP NESTED FIELDS

type OfferField1
    = Name (Form.Types.Field String)

-- OUTPUT OF THE FORM

type alias Output =
    { field1 : Int
    , offer : Offer
    , field3 : List String
    , field4 : List Offer
    }

-- OUTPUT OF THE NESTED FORM 

type alias Offer =
    { name : String
    }


type alias Form =
    Form.Types.Form () Field Output

-- OFFER VALIDATION

offerValidation : Form.Types.Validation () OfferField1 Offer
offerValidation =
    succeed Offer
        |> andMap (fromString Name string)

-- MAIN VALIDATION

validation : Form.Types.Validation () Field Output
validation =
    succeed Output
        |> andMap (fromString Field1 int)
        |> andMap
            (fromNested OfferField -- USING NESTED VALIDATION
                (\x -> -- UNFORTUNATE BUT NECESSARY BOILERPLATE
                    case x of
                        OfferField field ->
                            Just field

                        _ ->
                            Nothing
                )
                offerValidation
            )
        |> andMap (fromStringList Field3 string )
        |> andMap
            (fromNestedList NestedOffers
                (\x ->
                    case x of
                        NestedOffers i x ->
                            Just ( i, x )

                        _ ->
                            Nothing
                )
                offerValidation
            )

-- INIT AND SETTING UP INITIAL VALUES

init : Form
init =
    Form.Form.form validation
        |> Form.Fields.transaction
        |> Form.Fields.setFieldString Field1 "1"
        |> Form.Fields.setFieldString (OfferField << Name) "name"
        |> Form.Fields.setFieldString (Field3 0) "1"
        |> Form.Fields.setFieldString (NestedOffers 0 << Name) "name2"
        |> Form.Fields.save



-- view : Form -> Html FormMsg
-- view form =
--     Form.view form
--         [ textInput Field1 []
--         ,
--         ]
-- viewOffer : List (FormHtml )
