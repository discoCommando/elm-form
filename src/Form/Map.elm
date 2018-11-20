module Form.Map exposing (Map, empty, exists, filterMap, filterMapList, get, length, mapKey, mapValue, mergeWith, set, toList)


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
            case state.key == key of
                True ->
                    M (State key value :: rest)

                False ->
                    case set key value (M rest) of
                        M new ->
                            M (state :: new)


exists : key -> Map key value -> Bool
exists key map =
    case map of
        M [] ->
            False

        M (state :: rest) ->
            case state.key == key of
                True ->
                    True

                False ->
                    exists key (M rest)


get : key -> Map key value -> Maybe value
get key map =
    case map of
        M [] ->
            Nothing

        M (state :: rest) ->
            case state.key == key of
                True ->
                    Just state.value

                False ->
                    get key (M rest)


mapKey : (key1 -> key2) -> Map key1 value -> Map key2 value
mapKey f map =
    case map of
        M list ->
            M (list |> List.map (\state -> { key = f state.key, value = state.value }))


mapValue : (value1 -> value2) -> Map key value1 -> Map key value2
mapValue f map =
    case map of
        M list ->
            M (list |> List.map (\state -> { key = state.key, value = f state.value }))


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


filterMapList : (key1 -> Maybe ( Int, key2 )) -> Map key1 value -> List (Map key2 value)
filterMapList f map =
    case map of
        M list ->
            let
                intMap =
                    list
                        |> List.foldl
                            (\state acc ->
                                case state.key |> f of
                                    Nothing ->
                                        acc

                                    Just ( i, key2 ) ->
                                        let
                                            intList =
                                                acc |> get i |> Maybe.withDefault []
                                        in
                                        acc |> set i ({ key = key2, value = state.value } :: intList)
                            )
                            empty
            in
            case intMap of
                M intList ->
                    intList |> List.sortBy .key |> List.map (.value >> M)


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
