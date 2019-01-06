module Form.FieldIndex
    exposing
        ( FieldIndex
        , Values
        , create
        , equals
        , fromString
        , get
        , map
        , next
        , remove
        , set
        , toString
        )

import Dict exposing (Dict)
import Form.UniqueIndex as UIdx


type FieldIndex
    = FieldIndex UIdx.UniqueIndex


create : FieldIndex
create =
    FieldIndex UIdx.create


next : FieldIndex -> FieldIndex
next (FieldIndex uidx) =
    FieldIndex (UIdx.next uidx)


toString : FieldIndex -> String
toString (FieldIndex uidx) =
    uidx |> UIdx.toString


fromString : String -> Maybe FieldIndex
fromString s =
    UIdx.fromString s |> Maybe.map FieldIndex


equals : FieldIndex -> FieldIndex -> Bool
equals (FieldIndex u1) (FieldIndex u2) =
    u1 |> UIdx.equals u2


type alias Values x =
    Dict String x


get : FieldIndex -> Values x -> Maybe x
get fi values =
    values |> Dict.get (fi |> toString)


map : (x -> y) -> Values x -> Values y
map f values =
    Dict.map (\_ -> f) values


set : FieldIndex -> x -> Values x -> Values x
set fieldIndex x values =
    values |> Dict.insert (fieldIndex |> toString) x


empty : Values x
empty =
    Dict.empty


remove : FieldIndex -> Values x -> Values x
remove fieldIndex xValues =
    xValues |> Dict.remove (fieldIndex |> toString)
