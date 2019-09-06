module Form.View2 exposing(..)

import Form.Field as Field
import Form.Get as Get
import Form.Transaction exposing (Transaction)
import Form.Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Index.UniqueIndex exposing (UniqueIndex)
import Form.FieldState as FieldState

type View error field result 
    = VI_STRING (Field.Value String -> field) (Get.ValueState String -> FieldState.ErrorState error -> result)
    | VI_PURE result 

type Submitted
    = Submitted
    | NotSubmitted


type alias Form error field output =
    Form.Validation.Form error field output Submitted

mapField : (field1 -> field2) -> View error field1 result -> View error field2 result 
mapField f view = 
    case view of 
        VI_STRING fieldF cont -> 
            VI_STRING (fieldF >> f) cont 

        VI_PURE res -> 
            VI_PURE res 


map : (result1 -> result2) -> View error field result1 -> View error field result2 
map f view = 
    case view of 
        VI_STRING fieldF cont -> 
            VI_STRING fieldF (\gs fe -> cont gs fe |> f)

        VI_PURE res -> 
            VI_PURE (res |> f)

mapBoth : (field1 -> field2) -> (result1 -> result2) -> View error field1 result1 -> View error field2 result2
mapBoth ff fr = 
    mapField ff >> map fr 

nested : (Field.Nested field1 -> field2) -> View error field1 result -> View error field2 result 
nested fieldNestedF = 
    mapField (\field1 -> Field.WithValue field1 |> fieldNestedF)


inIndex : UniqueIndex -> (Field.List field1 -> field2) -> View error field1 result -> View error field2 result 
inIndex uniqueIndex fieldListF = 
    mapField (\field1 -> Field.WithIndex uniqueIndex field1 |> fieldListF)


inForm : Form error field output -> View error field result -> result 
inForm form view = 
    case view of 
        VI_STRING fieldF cont -> 
            let 
                value = Get.getString (Get.field fieldF) form 
                error = Get.getError (Get.field fieldF) form 
            in
            cont value error 

        VI_PURE result -> 
            result 

stringInput : 
    (Field.Value String -> field) 
    -> ({ msg : String -> Transaction field
        , value: Get.ValueState String 
        , error : FieldState.ErrorState error 
        } -> result) 
    -> View error field result
stringInput field f =
    VI_STRING field (\value error -> f { msg = (Form.Transaction.setString field), value = value, error = error })
