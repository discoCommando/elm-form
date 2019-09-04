module View2Example exposing (..)


import Form 
import Form.Validation exposing (..)
import Form.Field as Field 
import Form.Get as Get
import Form.CommonError exposing (CommonError)
import Html 
import Form.View.Html 
import Form.Transaction as Transaction 

type Field 
	= FirstName (Field.Value String)
	| Number (Field.Value String)

type alias Output = { firstName: String, number: Maybe Int }

type alias Form =
    Form.Form CommonError Field Result 


type alias Validation output =
    Form.Validation CommonError Field output


validation : Validation Output 
validation = 
	succeed Output 
		|> andMap (fromString FirstName <| required string)
		|> andMap (fromString Number <| optional int)


form : Form 
form = Form.form validation 


type alias Model = { form : Form }

type Msg = T (Transaction Field)

update : Msg -> Model -> Model 
update msg model =
	case msg of 
		T transaction -> 
			{ form = model.form |> Transaction.save transaction }