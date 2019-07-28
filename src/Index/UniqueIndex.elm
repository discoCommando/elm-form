module Index.UniqueIndex exposing (UniqueIndex, create, equals, fromString, next, toString)


type UniqueIndex
    = UniqueIndex Int


create : UniqueIndex
create =
    UniqueIndex 0


next : UniqueIndex -> UniqueIndex
next (UniqueIndex i) =
    UniqueIndex (i + 1)


toString : UniqueIndex -> String
toString (UniqueIndex i) =
    String.fromInt i


fromString : String -> Maybe UniqueIndex
fromString s =
    String.toInt s |> Maybe.map UniqueIndex


equals : UniqueIndex -> UniqueIndex -> Bool
equals (UniqueIndex i1) (UniqueIndex i2) =
    i1 == i2
