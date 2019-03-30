module Deck exposing (Card, Deck, after, before, current, fromList, insert, isEmpty, mapCurrent, moveDelta, next, previous, remove, select, sort, sortBy, toList)

import List.Extra as ListExtra
import List.Zipper as LZ exposing (Zipper)
import Types exposing (..)


type Deck
    = Deck (Maybe (Zipper Card))


type alias Card =
    { id : Int, title : String, content : String, position : Int, otherPosition : Maybe Int }


fromList : List Card -> Deck
fromList list =
    Deck (LZ.fromList list)


toList : Deck -> List Card
toList (Deck mbz_) =
    case mbz_ of
        Just zipper ->
            zipper |> LZ.toList

        Nothing ->
            []


isEmpty : Deck -> Bool
isEmpty (Deck mbz_) =
    mbz_ == Nothing



-- MODIFIERS


moveDelta : Int -> Deck -> Deck
moveDelta delta (Deck zip_) =
    let
        ( zipPart, maybeReverse, endValue ) =
            if delta < 0 then
                ( LZ.before, List.reverse, minInt )

            else
                ( LZ.after, identity, maxInt )

        ( prev1_, prev2_ ) =
            ( zip_
                |> Maybe.map zipPart
                |> Maybe.map maybeReverse
                |> Maybe.andThen (ListExtra.getAt (abs delta - 1))
            , zip_
                |> Maybe.map zipPart
                |> Maybe.map maybeReverse
                |> Maybe.andThen (ListExtra.getAt (abs delta))
            )

        updatePosFn =
            case ( prev1_, prev2_ ) of
                ( Just prev1, Just prev2 ) ->
                    \c -> { c | position = (prev1.position + prev2.position) // 2 }

                ( Just prev1, Nothing ) ->
                    \c -> { c | position = (prev1.position + endValue) // 2 }

                _ ->
                    identity
    in
    zip_
        |> Maybe.map (LZ.mapCurrent updatePosFn)
        |> Deck
        |> sort


insert : Card -> Deck -> Deck
insert newCurrent ((Deck zip_) as deck) =
    case zip_ of
        Just zip ->
            (LZ.toList zip ++ [ newCurrent ])
                |> LZ.fromList
                |> Maybe.map LZ.last
                |> Deck

        Nothing ->
            Deck (LZ.fromList [ newCurrent ])


remove : Deck -> Deck
remove ((Deck zip_) as deck) =
    case zip_ of
        Just zip ->
            let
                nextSelected =
                    getNext zip

                preBefore =
                    LZ.before zip

                preAfter =
                    LZ.after zip
            in
            (preBefore ++ preAfter)
                |> LZ.fromList
                |> Maybe.andThen (LZ.findFirst <| (==) nextSelected)
                |> Deck

        Nothing ->
            deck


sort : Deck -> Deck
sort ((Deck zip_) as deck) =
    case zip_ of
        Just zip ->
            let
                presortCurrent =
                    LZ.current zip
            in
            zip
                |> LZ.toList
                |> List.sortBy .position
                |> LZ.fromList
                |> Maybe.andThen (LZ.findFirst <| (==) presortCurrent)
                |> Deck

        Nothing ->
            deck


sortBy : (Card -> comparable) -> Deck -> Deck
sortBy comp ((Deck zip_) as deck) =
    case zip_ of
        Just zip ->
            let
                presortCurrent =
                    LZ.current zip
            in
            zip
                |> LZ.toList
                |> List.sortBy comp
                |> LZ.fromList
                |> Maybe.andThen (LZ.findFirst (\i -> i == presortCurrent))
                |> Deck

        Nothing ->
            deck


mapCurrent : (Card -> Card) -> Deck -> Deck
mapCurrent mapFn (Deck zip_) =
    zip_
        |> Maybe.map (LZ.mapCurrent mapFn)
        |> Deck



-- ACCESSORS


current : Deck -> Maybe Card
current (Deck mbz_) =
    case mbz_ of
        Nothing ->
            Nothing

        Just zip ->
            Just (LZ.current zip)


before : Deck -> Maybe (List Card)
before (Deck zip_) =
    Maybe.map LZ.before zip_


after : Deck -> Maybe (List Card)
after (Deck zip_) =
    Maybe.map LZ.after zip_



-- SELECTORS


select : (Card -> Bool) -> Deck -> Deck
select predicate (Deck mbz_) =
    case mbz_ of
        Nothing ->
            Deck Nothing

        Just zip ->
            zip
                |> LZ.findFirst predicate
                |> Deck


next : Deck -> Deck
next (Deck zip_) =
    let
        newZip_ =
            zip_ |> Maybe.andThen LZ.next
    in
    case newZip_ of
        Nothing ->
            Deck zip_

        Just newZip ->
            Deck (Just newZip)


previous : Deck -> Deck
previous (Deck zip_) =
    let
        newZip_ =
            zip_ |> Maybe.andThen LZ.previous
    in
    case newZip_ of
        Nothing ->
            Deck zip_

        Just newZip ->
            Deck (Just newZip)



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
