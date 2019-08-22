module Form.Type exposing (Form, UniqueIndexDictState)

import Form.FieldState exposing (InternalFieldState)
import Form.Map as Map exposing (Map)
import Index.FieldIndex as FieldIndex exposing (..)
import Index.FieldIndexDict as FieldIndexDict exposing (..)
import Index.UniqueIndex as UniqueIndex exposing (..)
import Index.UniqueIndexDict as UniqueIndexDict exposing (..)


type alias UniqueIndexDictState =
    { fieldIndexSet : FieldIndexDict ()
    , order : Int -- made for the order
    }


type alias Form error field output validation submitted =
    { fieldIndexes : Map field FieldIndex
    , listIndexes : FieldIndexDict (UniqueIndexDict UniqueIndexDictState)
    , values : FieldIndexDict (InternalFieldState error)
    , submitted : submitted
    , validation : validation -- Validation error field output
    , output : Maybe output
    , fieldIndexToUse : FieldIndex
    , uniqueIndexToUse : UniqueIndex
    , counter : Int
    }
