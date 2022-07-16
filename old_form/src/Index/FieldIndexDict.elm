module Index.FieldIndexDict exposing (FieldIndexDict, empty, get, keys, map, remove, set, update)

import Dict exposing (Dict)
import Index.FieldIndex as FieldIndex exposing (FieldIndex)


type alias FieldIndexDict x =
    Dict String x


get : FieldIndex -> FieldIndexDict x -> Maybe x
get fi values =
    values |> Dict.get (fi |> FieldIndex.toString)


map : (x -> y) -> FieldIndexDict x -> FieldIndexDict y
map f values =
    Dict.map (\_ -> f) values


set : FieldIndex -> x -> FieldIndexDict x -> FieldIndexDict x
set fieldIndex x values =
    values |> Dict.insert (fieldIndex |> FieldIndex.toString) x


update : FieldIndex -> (Maybe x -> Maybe x) -> FieldIndexDict x -> FieldIndexDict x
update fieldIndex fun values =
    values |> Dict.update (fieldIndex |> FieldIndex.toString) fun


empty : FieldIndexDict x
empty =
    Dict.empty


remove : FieldIndex -> FieldIndexDict x -> FieldIndexDict x
remove fieldIndex xFieldIndexDict =
    xFieldIndexDict |> Dict.remove (fieldIndex |> FieldIndex.toString)


keys : FieldIndexDict x -> List FieldIndex
keys =
    Dict.keys >> List.filterMap FieldIndex.fromString
