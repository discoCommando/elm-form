module Form exposing
    ( FailCell
    , FailState
    , Field(..)
    , FieldList(..)
    , FieldNested(..)
    , Form
    , SuccessState
    , Transaction(..)
    , UniqueIndexDictState
    , Validation(..)
    , ValidationAction(..)
    , ValidationResult(..)
    , ValidationResultCell
    , View(..)
    , clearErrors
    , field
    , fieldNested
    , fieldNestedNotOpaque
    , form
    , fromFailState
    , fromSuccess
    , getFieldIndex
    , getOutput
    , indexes
    , listField
    , listOpaque
    , mapFailState
    , merge
    , setErrors
    , validate
    , validateHelper
    , Get(..)
    )

-- COMPOSABLE
-- TYPE SAFE

import Form.FieldState exposing (FieldState)
import Form.Map as Map exposing (Map)
import Html exposing (Html)
import Index.FieldIndex as FieldIndex exposing (..)
import Index.FieldIndexDict as FieldIndexDict exposing (..)
import Index.UniqueIndex as UniqueIndex exposing (..)
import Index.UniqueIndexDict as UniqueIndexDict exposing (..)
import Form.Validation exposing (validate_, Validation)

type Transaction field
    = T_STR (Field String -> field) String
    | T_ADDROW field (UniqueIndex -> Transaction field)
    | T_SETINLIST field UniqueIndex (Transaction field)
    | T_REMOVEROW field UniqueIndex
    | T_BATCH (List (Transaction field))

type View error field msg
    = VI_STRING (Field String -> field) (String -> Maybe error -> Html msg)
    | VI_HTML (Html msg)
    | VI_VIEW String (List (Html.Attribute msg)) (List (View error field msg))
    | VI_REMOVELASTROW field (Maybe UniqueIndex -> Html msg)
    | VI_INLIST field (List UniqueIndex -> View error field msg)
    | VI_LAZY (() -> View error field msg)

type Get field resultType = Get (Field resultType -> field)


listOpaque : (FieldList a -> field) -> field
listOpaque listF =
    listF OpaqueList


listField : (FieldList a -> field) -> UniqueIndex -> a -> field
listField listF i a =
    listF (WithIndex i a)


field : (Field a -> field) -> field
field fieldF =
    fieldF Field


fieldNested : (FieldNested a -> field) -> field
fieldNested fieldF =
    fieldF OpaqueNested


fieldNestedNotOpaque : (FieldNested a -> field) -> a -> field
fieldNestedNotOpaque fieldF a =
    fieldF (WithValue a)


type alias UniqueIndexDictState =
    { fieldIndexSet : FieldIndexDict ()
    , order : Int -- made for the order
    }


type alias Form error field output =
    { fieldIndexes : Map field FieldIndex
    , listIndexes : FieldIndexDict (UniqueIndexDict UniqueIndexDictState)
    , values : FieldIndexDict (FieldState error)
    , submitted : Bool
    , validation : Validation error field output
    , output : Maybe output
    , fieldIndexToUse : FieldIndex
    , uniqueIndexToUse : UniqueIndex
    , counter : Int
    }


form : Validation field error output -> Form field error output
form validation =
    { fieldIndexes = Map.empty
    , listIndexes = FieldIndexDict.empty
    , values = FieldIndexDict.empty
    , submitted = False
    , validation = validation
    , output = Nothing
    , fieldIndexToUse = FieldIndex.create
    , uniqueIndexToUse = UniqueIndex.create
    , counter = 0
    }
        |> validate


validate : Form error field output -> Form error field output
validate form_ =
    

mapFailState : (field1 -> field2) -> FailState error field1 -> FailState error field2
mapFailState mapf failState =
    { errors = failState.errors |> Map.mapBoth (\key failCell -> ( key |> mapf, failCell )) }



getFieldIndex : field -> Form error field output -> ( Form error field output, FieldIndex )
getFieldIndex field_ form_ =
    case form_.fieldIndexes |> Map.get field_ of
        Nothing ->
            ( { form_
                | fieldIndexes = form_.fieldIndexes |> Map.set field_ form_.fieldIndexToUse
                , fieldIndexToUse = form_.fieldIndexToUse |> FieldIndex.next
              }
            , form_.fieldIndexToUse
            ) 

        Just fi ->
            ( form_, fi )


getOutput : Form error field output -> Maybe output
getOutput form_ =
    form_.output
