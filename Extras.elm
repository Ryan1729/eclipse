module Extras exposing (..)

import Random.Pcg as Random exposing (Seed)


shuffle : Seed -> List a -> List a
shuffle seed list =
    let
        length =
            List.length list

        randomTags =
            Random.step (Random.list length (Random.int 0 length)) seed
                |> fst
    in
        List.map2 (,) randomTags list |> List.sortBy fst |> List.unzip |> snd



-- from https://github.com/elm-community/list-extra


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest



-- from https://github.com/elm-community/maybe-extra


orElseLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orElseLazy fma mb =
    case mb of
        Nothing ->
            fma ()

        Just _ ->
            mb
