module Form.Field exposing (Field(..), FieldList(..), FieldNested(..), Text(..))

type Text = Text (Field String)

type Field a
    = Field


type FieldList a
    = OpaqueList
    | WithIndex UniqueIndex a


type FieldNested a
    = OpaqueNested
    | WithValue a
