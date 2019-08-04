module Form.Type exposing (..)

import Form.FieldState exposing (FieldState)
import Index.FieldIndex as FieldIndex exposing (..)
import Index.FieldIndexDict as FieldIndexDict exposing (..)
import Index.UniqueIndex as UniqueIndex exposing (..)
import Index.UniqueIndexDict as UniqueIndexDict exposing (..)
import Form.Map as Map exposing (Map)


type alias UniqueIndexDictState =
    { fieldIndexSet : FieldIndexDict ()
    , order : Int -- made for the order
    }

type alias Form error field output validation =
    { fieldIndexes : Map field FieldIndex
    , listIndexes : FieldIndexDict (UniqueIndexDict UniqueIndexDictState)
    , values : FieldIndexDict (FieldState error)
    , submitted : Bool
    , validation : validation -- Validation error field output
    , output : Maybe output
    , fieldIndexToUse : FieldIndex
    , uniqueIndexToUse : UniqueIndex
    , counter : Int
    }
