module ComplexForm exposing (Email(..), Field(..), Form, Password(..), Plan(..), UserDetails, Validation, form, parseEmail, parsePassword, parsePlan, passwordValidation, validation)

import Form
import Form.CommonError as CommonError exposing (CommonError(..))
import Form.Field as Field
import Form.Validation
    exposing
        ( andMap
        , andMapDiscard
        , andThen
        , anyString
        , failure
        , fromBool
        , fromString
        , isSubmitted
        , isTrue
        , map
        , mapError
        , nonEmptyString
        , optional
        , required
        , succeed
        )


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
    | AgreedToTerms (Field.Value Bool)
    | Submitted (Field.Value Bool)


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


passwordValidation : Form.Validation.Submitted -> Validation Password
passwordValidation submitted =
    succeed (\a b -> ( a, b ))
        |> andMap (fromString Password <| required submitted parsePassword)
        |> andMap (fromString RepeatPassword <| required submitted anyString)
        |> andThen
            (\( Password_ p, rp ) ->
                if p == rp then
                    Password_ p |> succeed

                else
                    failure RepeatPassword <| CommonError.custom "The passwords must match"
            )


validation : Validation UserDetails
validation =
    isSubmitted Submitted
        |> andThen
            (\submitted ->
                succeed UserDetails
                    |> andMap (fromString Name <| optional anyString)
                    |> andMap (fromString Email <| required submitted parseEmail)
                    |> andMap (passwordValidation submitted)
                    |> andMapDiscard (isTrue AgreedToTerms submitted)
                    |> andMap (fromString Plan <| required submitted parsePlan)
            )


form : Form
form =
    Form.form validation



-- validate on blur
-- add bools
-- add isTrue for submitted
-- add required that takes boolean (is submitted)
