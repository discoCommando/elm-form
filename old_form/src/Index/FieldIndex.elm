module Index.FieldIndex exposing
    ( FieldIndex
    , create
    , equals
    , fromString
    , next
    , toString
    )

import Index.UniqueIndex as UniqueIndex


type FieldIndex
    = FieldIndex UniqueIndex.UniqueIndex


create : FieldIndex
create =
    FieldIndex UniqueIndex.create


next : FieldIndex -> FieldIndex
next (FieldIndex uidx) =
    FieldIndex (UniqueIndex.next uidx)


toString : FieldIndex -> String
toString (FieldIndex uidx) =
    uidx |> UniqueIndex.toString


fromString : String -> Maybe FieldIndex
fromString s =
    UniqueIndex.fromString s |> Maybe.map FieldIndex


equals : FieldIndex -> FieldIndex -> Bool
equals (FieldIndex u1) (FieldIndex u2) =
    u1 |> UniqueIndex.equals u2
