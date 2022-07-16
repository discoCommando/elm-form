module Index.UniqueIndexDict exposing (UniqueIndexDict, empty, get, keys, map, remove, set, toList, values)

import Dict exposing (Dict)
import Index.UniqueIndex as UniqueIndex exposing (UniqueIndex)


type alias UniqueIndexDict x =
    Dict String x


get : UniqueIndex -> UniqueIndexDict x -> Maybe x
get fi =
    Dict.get (fi |> UniqueIndex.toString)


map : (x -> y) -> UniqueIndexDict x -> UniqueIndexDict y
map f =
    Dict.map (\_ -> f)


set : UniqueIndex -> x -> UniqueIndexDict x -> UniqueIndexDict x
set fieldIndex x =
    Dict.insert (fieldIndex |> UniqueIndex.toString) x


empty : UniqueIndexDict x
empty =
    Dict.empty


remove : UniqueIndex -> UniqueIndexDict x -> UniqueIndexDict x
remove fieldIndex xUniqueIndexDict =
    xUniqueIndexDict |> Dict.remove (fieldIndex |> UniqueIndex.toString)


keys : UniqueIndexDict x -> List UniqueIndex
keys =
    Dict.keys >> List.filterMap UniqueIndex.fromString


values : UniqueIndexDict x -> List x
values =
    Dict.values


toList : UniqueIndexDict x -> List ( UniqueIndex, x )
toList =
    Dict.toList
        >> List.filterMap
            (\( s, x ) ->
                UniqueIndex.fromString s |> Maybe.map (\ui -> ( ui, x ))
            )
