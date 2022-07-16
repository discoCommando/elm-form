module Select2 exposing (..)

import Form 
import Form.Transaction as Transaction 
import Form.Validation as Validation 
import Form.Field as Field 
import Form.CommonError as CommonError exposing (CommonError)

type Field = 
	One (Field.Value String)


type alias Output = { something : String }

validation : Form.Validation 