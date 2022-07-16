module PhilFreeman exposing (..)


import Form 
import Form.Validation exposing (..)
import Form.Field as Field 
import Form.Get as Get
import Form.CommonError exposing (CommonError)

type Field
    = FirstName (Field.Value String)
    | LastName (Field.Value String)
    | Email (Field.Value String)
    | Other (Field.Value String)


type Result =
    Empty
    | Required RequiredUserDetails

type alias RequiredUserDetails = 
    { firstName: String, lastName: String, email: String, other: String }


type alias Form =
    Form.Form CommonError Field Result 


type alias Validation output =
    Form.Validation CommonError Field output

isEmpty : String -> Result.Result error Bool 
isEmpty = 
    String.isEmpty >> Ok 

validation : Validation Result 
validation = 
    let
        fieldsToCheck = 
            [ FirstName
            , LastName 
            , Email 
            , Other 
            ] 
    in 
    traverse (fieldsToCheck |> List.map (\field -> fromString field isEmpty))
        |> map (List.all identity)
        |> andThen (\allEmpty -> 
            if allEmpty then 
                succeed Empty 
            else 
                succeed RequiredUserDetails
                    |> andMap (fromString FirstName <| required nonEmptyString)
                    |> andMap (fromString LastName <| required nonEmptyString)
                    |> andMap (fromString Email <| required nonEmptyString)
                    |> andMap (fromString Other <| required nonEmptyString)
                    |> map Required
            )
