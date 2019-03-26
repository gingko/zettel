module Deck exposing (Deck, fromList, move, moveWithPosition, next, previous, sort, toList)

import List.Zipper as LZ exposing (Zipper(..))
import Types exposing (..)


type Deck a
    = Deck (Maybe (Zipper ( a, Positions )))


type alias Positions =
    { herePosition : Int, otherPosition : Maybe Int }


move : (a -> b) -> ( Deck a, Deck b ) -> ( Deck a, Deck b )
move conv ( fromDeck, toDeck ) =
    case ( fromDeck, toDeck ) of
        ( Deck Nothing, Deck mbzTo_ ) ->
            ( fromDeck, toDeck )

        ( Deck (Just (Zipper bef ( curr, currPos ) aft)), Deck Nothing ) ->
            ( bef ++ aft |> LZ.fromList |> Deck
            , Deck (Just (Zipper [] ( conv curr, { herePosition = currPos.otherPosition |> Maybe.withDefault 0, otherPosition = Just currPos.herePosition } ) []))
            )

        ( Deck (Just (Zipper bef ( curr, currPos ) aft)), Deck (Just (Zipper befTo currTo aftTo)) ) ->
            ( bef ++ aft |> LZ.fromList |> Deck
            , Deck (Just (Zipper (befTo ++ [ currTo ]) ( conv curr, { herePosition = currPos.otherPosition |> Maybe.withDefault 0, otherPosition = Just currPos.herePosition } ) aftTo))
            )


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
            -(2 ^ 31)

        maxInt =
            (2 ^ 31) - 1

        mapFn : Int -> a -> ( a, { herePosition : Int, otherPosition : Maybe Int } )
        mapFn i c =
            ( c
            , { herePosition = minInt + ((maxInt - minInt) // (length + 1)) * (i + 1)
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
