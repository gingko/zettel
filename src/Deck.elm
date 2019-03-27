module Deck exposing (Deck, current, fromList, isEmpty, move, moveWithPosition, next, previous, select, sort, toList)

import List.Zipper as LZ exposing (Zipper(..))
import Types exposing (..)


type Deck a
    = Deck (Maybe (Zipper ( a, Positions )))


type alias Positions =
    { herePosition : Int, otherPosition : Maybe Int }


move : (a -> b) -> ( Deck a, Deck b ) -> ( Deck a, Deck b )
move conv ( fromDeck, toDeck ) =
    case ( fromDeck, toDeck ) of
        ( Deck (Just zipFrom), Deck (Just zipTo) ) ->
            let
                toMove =
                    LZ.current zipFrom

                ( newFromCurrent, _ ) =
                    getNext zipFrom

                newToBefore =
                    LZ.before zipTo ++ [ LZ.current zipTo ]

                newToCurrent =
                    Tuple.mapFirst conv toMove

                newToAfter =
                    LZ.after zipTo
            in
            ( zipFrom
                |> LZ.toList
                |> List.filter (\i -> i /= toMove)
                |> LZ.fromList
                |> Deck
                |> select (\i -> i == newFromCurrent)
            , LZ.from newToBefore newToCurrent newToAfter
                |> Just
                |> Deck
            )

        ( Deck (Just zipFrom), Deck Nothing ) ->
            let
                toMove =
                    LZ.current zipFrom

                ( newFromCurrent, _ ) =
                    getNext zipFrom

                newToCurrent =
                    Tuple.mapFirst conv toMove
            in
            ( zipFrom
                |> LZ.toList
                |> List.filter (\i -> i /= toMove)
                |> LZ.fromList
                |> Deck
                |> select (\i -> i == newFromCurrent)
            , LZ.fromCons newToCurrent []
                |> Just
                |> Deck
            )

        ( Deck Nothing, _ ) ->
            ( fromDeck, toDeck )


moveWithPosition : (a -> b) -> RelativePosition Int -> ( Deck a, Deck b ) -> ( Deck a, Deck b )
moveWithPosition conv pos ( fromDeck, toDeck ) =
    ( fromDeck, toDeck )


current : Deck a -> Maybe a
current (Deck mbz_) =
    case mbz_ of
        Nothing ->
            Nothing

        Just zip ->
            Just (LZ.current zip |> Tuple.first)


select : (a -> Bool) -> Deck a -> Deck a
select predicate (Deck mbz_) =
    case mbz_ of
        Nothing ->
            Deck Nothing

        Just zip ->
            zip
                |> LZ.findFirst (Tuple.first >> predicate)
                |> Deck


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


isEmpty : Deck a -> Bool
isEmpty (Deck mbz_) =
    mbz_ == Nothing



-- PRIVATE HELPERS


getNext : Zipper a -> a
getNext zip =
    case ( LZ.next zip, LZ.previous zip ) of
        ( Just nextZip, _ ) ->
            LZ.current nextZip

        ( Nothing, Just prevZip ) ->
            LZ.current prevZip

        ( Nothing, Nothing ) ->
            LZ.current zip
