module ComplexForm exposing (Email(..), Field(..), Form, Password(..), Plan(..), UserDetails, emailValidation, parsePassword, validation)

import Form
import Form.Field as Field
import Form.Validation exposing (andMap, andThen, failure, fromString, map, optional, string, succeed, andMapDiscard, mapError)


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
    Form.Form String Field UserDetails


emailValidation : String -> Result String Email
emailValidation s =
    if String.contains "@" s then
        Ok <| Email_ s

    else
        Err "Invalid email"


parsePassword s =
    if String.length s >= 6 then
        Ok <| Password_ s

    else
        Err "Password must be at least 6 characters"

parsePlan s =
    case s of
        "Basic" ->
            Ok Basic

        "Pro" ->
            Ok Pro

        "Enterprise" ->
            Ok Enterprise

        _ ->
            Err "Invalid plan"


validation : Form.Validation String Field UserDetails
validation =
    succeed UserDetails
        |> andMap (fromString Name (string |> optional) |> mapError (\_ -> ""))
        |> andMap (fromString Email emailValidation)
        |> andMap
            (fromString Password parsePassword
                |> andThen
                    (\(Password_ p) ->
                        fromString RepeatPassword parsePassword
                            |> andThen
                                (\(Password_ rp) ->
                                    if p == rp then
                                        succeed <| Password_ p

                                    else
                                        failure RepeatPassword "The passwords must match"
                                )
                    )
            )
        |> andMapDiscard (fromString AgreedToTerms (string >> Result.map (\s -> s == "1")) |> mapError (\_ -> "No value"))
        |> andMap (fromString Plan parsePlan)

form : Form 
form = Form.form validation 

