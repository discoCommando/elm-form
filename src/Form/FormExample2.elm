module FormExample2 exposing (..)

import Form1


type Field
    = Field1 (Form1.Field String)
    | Offer (Form1.Nested OfferField)
      -- | Field3 (Form1.FieldList (Form1.Field String))
    | Field3 Int (Form1.Field String)
    | NestedOffers Int (Form1.Nested OfferField)


type OfferField
    = Name (Field String)


type alias Offer =
    { name : String
    }


type alias Output =
    { field1 : Int
    , offer : Offer
    , field3 : List String
    , field4 : List Offer
    }


type alias Form =
    Form1.Form String Field Output


offerValidation : Form1.Validation OfferField Offer
offerValidation =
    succeed Offer
        |> andMap (field Name string)


validation : Form1.Validation Field Output
validation =
    succeed Output
        |> andMap (field Field1 int)
        |> andMap (fieldNested Field2 offerValidation)
        |> andMap (fieldList Field3 string)
        |> andMap (fieldList Field4 offerValidation)


init : Form1.Form
init =
    Form1.form validation
        -- |> Form1.transaction
        |> Form1.setFieldString Field1 "1"
        |> Form1.setFieldNested Offer
            (\offerForm ->
                offerForm
                    |> Form1.setFieldString Name "name"
            )
        |> Form1.setFieldString (Field3 0) "1"
        |> Form1.setFieldNested (NestedOffers 0) (Form1.setFieldString Name "name")



-- |> Form1.save


get1 : F
