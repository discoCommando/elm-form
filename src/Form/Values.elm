module Form.Values exposing (..)

import Dict exposing (Dict)
import Form.FieldIndex as FieldIndex exposing (FieldIndex)


type alias Values x =
    Dict String x


get : FieldIndex -> Values x -> Maybe x
get fi values =
    values |> Dict.get (fi |> FieldIndex.toString)


map : (x -> y) -> Values x -> Values y
map f values =
    Dict.map (\_ -> f) values


set : FieldIndex -> x -> Values x -> Values x
set fieldIndex x values =
    values |> Dict.insert (fieldIndex |> FieldIndex.toString) x


empty : Values x
empty =
    Dict.empty


remove : FieldIndex -> Values x -> Values x
remove fieldIndex xValues =
    xValues |> Dict.remove (fieldIndex |> FieldIndex.toString)


keys : Values x -> List FieldIndex
keys =
    Dict.keys >> List.filterMap FieldIndex.fromString
