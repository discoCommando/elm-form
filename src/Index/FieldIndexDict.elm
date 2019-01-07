module Index.FieldIndexDict exposing (..)

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


empty : FieldIndexDict x
empty =
    Dict.empty


remove : FieldIndex -> FieldIndexDict x -> FieldIndexDict x
remove fieldIndex xFieldIndexDict =
    xFieldIndexDict |> Dict.remove (fieldIndex |> FieldIndex.toString)


keys : FieldIndexDict x -> List FieldIndex
keys =
    Dict.keys >> List.filterMap FieldIndex.fromString
