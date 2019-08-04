module Form.Field exposing (Value(..), List(..), Nested(..))

import Index.UniqueIndex exposing (UniqueIndex)

type Value a
    = Value


type List a
    = OpaqueList
    | WithIndex UniqueIndex a


type Nested a
    = OpaqueNested
    | WithValue a
