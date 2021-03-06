module Form.Map exposing
    ( Map
    , empty
    , exists
    , filter
    , filterMap
    , foldl
    , get
    , length
    , mapBoth
    , mapKey
    , mapValue
    , mergeWith
    , remove
    , set
    , singleton
    , toList
    , update
    , updateWithDefault
    )


type Map key value
    = M (List (State key value))


type alias State key value =
    { key : key
    , value : value
    }


empty : Map key value
empty =
    M []


set : key -> value -> Map key value -> Map key value
set key value map =
    case map of
        M [] ->
            M [ State key value ]

        M (state :: rest) ->
            if state.key == key then
                M (State key value :: rest)

            else
                case set key value (M rest) of
                    M new ->
                        M (state :: new)


exists : key -> Map key value -> Bool
exists key map =
    case map of
        M [] ->
            False

        M (state :: rest) ->
            if state.key == key then
                True

            else
                exists key (M rest)


get : key -> Map key value -> Maybe value
get key map =
    case map of
        M [] ->
            Nothing

        M (state :: rest) ->
            if state.key == key then
                Just state.value

            else
                get key (M rest)


remove : key -> Map key value -> Map key value
remove key map =
    case map of
        M [] ->
            M []

        M (state :: rest) ->
            if state.key == key then
                M rest

            else
                case remove key (M rest) of
                    M rest2 ->
                        M (state :: rest2)


filter : (key -> value -> Bool) -> Map key value -> Map key value
filter f map =
    case map of
        M [] ->
            M []

        M (state :: rest) ->
            case filter f (M rest) of
                M filtered ->
                    if f state.key state.value then
                        M (state :: filtered)

                    else
                        M filtered


update : key -> (value -> value) -> Map key value -> Map key value
update key updateF map =
    case map of
        M [] ->
            M []

        M (state :: rest) ->
            if state.key == key then
                M ({ state | value = state.value |> updateF } :: rest)

            else
                case update key updateF (M rest) of
                    M updated ->
                        M (state :: updated)


updateWithDefault : key -> (Maybe value -> value) -> Map key value -> Map key value
updateWithDefault key updateF map =
    case map of
        M [] ->
            M [ { key = key, value = updateF Nothing } ]

        M (state :: rest) ->
            if state.key == key then
                M ({ state | value = state.value |> Just |> updateF } :: rest)

            else
                case updateWithDefault key updateF (M rest) of
                    M updated ->
                        M (state :: updated)


mapKey : (key1 -> key2) -> Map key1 value -> Map key2 value
mapKey f map =
    case map of
        M list ->
            M (list |> List.map (\state -> { key = f state.key, value = state.value }))


singleton : key -> value -> Map key value
singleton key value =
    M [ { key = key, value = value } ]


mapValue : (value1 -> value2) -> Map key value1 -> Map key value2
mapValue f map =
    case map of
        M list ->
            M (list |> List.map (\state -> { key = state.key, value = f state.value }))


mapBoth : (key1 -> value1 -> ( key2, value2 )) -> Map key1 value1 -> Map key2 value2
mapBoth f map =
    case map of
        M list ->
            M
                (list
                    |> List.map
                        (\state ->
                            let
                                ( key2, value2 ) =
                                    f state.key state.value
                            in
                            { key = key2, value = value2 }
                        )
                )


foldl : (key -> value -> acc -> acc) -> acc -> Map key value -> acc
foldl f acc m =
    case m of
        M [] ->
            acc

        M (state :: rest) ->
            foldl f (f state.key state.value acc) (M rest)


filterMap : (key1 -> Maybe key2) -> Map key1 value -> Map key2 value
filterMap f map =
    case map of
        M list ->
            M
                (list
                    |> List.filterMap
                        (\state ->
                            case f state.key of
                                Just newKey ->
                                    Just { key = newKey, value = state.value }

                                Nothing ->
                                    Nothing
                        )
                )


mergeWith : Map key value -> Map key value -> Map key value
mergeWith map2 map1 =
    case map2 of
        M list ->
            list |> List.foldl (\state map -> set state.key state.value map) map1


toList : Map key value -> List ( key, value )
toList map =
    case map of
        M list ->
            list |> List.map (\state -> ( state.key, state.value ))


length : Map key value -> Int
length map =
    case map of
        M x ->
            List.length x
