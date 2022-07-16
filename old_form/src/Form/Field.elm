module Form.Field exposing (List(..), Nested(..), Value(..))

import Index.UniqueIndex exposing (UniqueIndex)


type Value a
    = Value


type List a
    = OpaqueList
    | WithIndex UniqueIndex a


type Nested a
    = OpaqueNested
    | WithValue a
