module Index.UniqueIndexSet exposing (UniqueIndexSet, insert, member, toList)

import Index.UniqueIndex as UniqueIndex exposing (UniqueIndex)
import Set exposing (Set)


type alias UniqueIndexSet =
    Set String


insert : UniqueIndex -> UniqueIndexSet -> UniqueIndexSet
insert uniqueIndex uniqueIndexSet =
    uniqueIndexSet |> Set.insert (uniqueIndex |> UniqueIndex.toString)


member : UniqueIndex -> UniqueIndexSet -> Bool
member uniqueIndex uniqueIndexSet =
    uniqueIndexSet |> Set.member (uniqueIndex |> UniqueIndex.toString)


toList : UniqueIndexSet -> List UniqueIndex
toList uniqueIndexSet =
    uniqueIndexSet |> Set.toList |> List.filterMap UniqueIndex.fromString
