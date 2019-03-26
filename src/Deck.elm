module Deck exposing (Deck, fromList, move, moveWithPosition, next, previous, sort, toList)

import List.Zipper as LZ exposing (Zipper)
import Types exposing (..)


type Deck a
    = Deck (Maybe (Zipper ( a, { herePosition : Int, otherPosition : Maybe Int } )))


move : (a -> b) -> ( Deck a, Deck b ) -> ( Deck a, Deck b )
move conv ( fromDeck, toDeck ) =
    ( fromDeck, toDeck )


moveWithPosition : (a -> b) -> RelativePosition Int -> ( Deck a, Deck b ) -> ( Deck a, Deck b )
moveWithPosition conv pos ( fromDeck, toDeck ) =
    ( fromDeck, toDeck )


next : Deck a -> Deck a
next deck =
    deck


previous : Deck a -> Deck a
previous deck =
    deck


sort : SortOrder -> Bool -> Deck a -> Deck a
sort sortOrder isReverse deck =
    deck


fromList : List a -> Deck a
fromList list =
    let
        length =
            List.length list

        minInt =
            -2 ^ 31

        maxInt =
            2 ^ 31 - 1

        mapFn : Int -> a -> ( a, { herePosition : Int, otherPosition : Maybe Int } )
        mapFn i c =
            ( c
            , { herePosition = minInt + (i + 1) * (maxInt - minInt) // (length + 1)
              , otherPosition = Nothing
              }
            )
    in
    list
        |> List.indexedMap mapFn
        |> LZ.fromList
        |> Deck


toList : Deck a -> List a
toList (Deck mbz_) =
    case mbz_ of
        Just zipper ->
            zipper |> LZ.toList |> List.map Tuple.first

        Nothing ->
            []
