module ComplexForm exposing (..)

import Form
import Form.Field as Field
import Form.Validation exposing (andMap, 
    andThen, failure, fromString, map, optional, anyString, nonEmptyString ,succeed, andMapDiscard, mapError, required)
import Form.CommonError as CommonError exposing (CommonError(..)) 

type Plan
    = Basic
    | Pro
    | Enterprise


type Email
    = Email_ String


type Password
    = Password_ String


type Field
    = Name (Field.Value String)
    | Email (Field.Value String)
    | Password (Field.Value String)
    | RepeatPassword (Field.Value String)
    | Plan (Field.Value String)
    | AgreedToTerms (Field.Value String)


type alias UserDetails =
    { name : Maybe String
    , email : Email
    , password : Password
    , plan : Plan
    }


type alias Form =
    Form.Form CommonError Field UserDetails

type alias Validation output =
    Form.Validation CommonError Field output 


parseEmail : String -> Result CommonError Email
parseEmail s =
    if String.contains "@" s then
        Ok <| Email_ s

    else
        Err <| CommonError.custom "Invalid email"


parsePassword s =
    if String.length s >= 6 then
        Ok <| Password_ s

    else
        Err <| CommonError.custom "Password must be at least 6 characters"

parsePlan s =
    case s of
        "Basic" ->
            Ok Basic

        "Pro" ->
            Ok Pro

        "Enterprise" ->
            Ok Enterprise

        _ ->
            Err <| CommonError.custom "Invalid plan"

passwordValidation : Validation Password 
passwordValidation =
    succeed (\a b -> (a,b)) 
        |> andMap (fromString Password <| required True parsePassword)
        |> andMap (fromString RepeatPassword <| required True anyString)
        |> andThen (\(Password_ p, rp) -> 
                if p == rp then 
                    Password_ p |> succeed 
                else 
                    failure RepeatPassword <| CommonError.custom "The passwords must match"
            )


validation : Validation UserDetails
validation =
    succeed UserDetails
        |> andMap (fromString Name <| optional anyString)
        |> andMap (fromString Email <| required True parseEmail)
        |> andMap passwordValidation
        |> andMapDiscard (fromString AgreedToTerms <| required True (anyString >> Result.map (\s -> s == "1")))
        |> andMap (fromString Plan <| required True parsePlan)

form : Form 
form = Form.form validation 


-- validate on blur
-- add bools
-- add isTrue for submitted 
-- add required that takes boolean (is submitted)
