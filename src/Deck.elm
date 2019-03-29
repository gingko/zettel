module Deck exposing (Deck, after, before, current, fromList, insert, isEmpty, map, mapCurrent, next, previous, remove, select, sort, sortBy, toList)

import List.Zipper as LZ exposing (Zipper)
import Types exposing (..)


type Deck a
    = Deck (Maybe (Zipper a))


fromList : List a -> Deck a
fromList list =
    Deck (LZ.fromList list)


toList : Deck a -> List a
toList (Deck mbz_) =
    case mbz_ of
        Just zipper ->
            zipper |> LZ.toList

        Nothing ->
            []


isEmpty : Deck a -> Bool
isEmpty (Deck mbz_) =
    mbz_ == Nothing



-- MODIFIERS


insert : a -> Deck a -> Deck a
insert newCurrent ((Deck zip_) as deck) =
    case zip_ of
        Just zip ->
            (LZ.toList zip ++ [ newCurrent ])
                |> LZ.fromList
                |> Maybe.map LZ.last
                |> Deck

        Nothing ->
            Deck (LZ.fromList [ newCurrent ])


remove : Deck a -> Deck a
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


sort : Deck comparable -> Deck comparable
sort ((Deck zip_) as deck) =
    case zip_ of
        Just zip ->
            let
                presortCurrent =
                    LZ.current zip
            in
            zip
                |> LZ.toList
                |> List.sort
                |> LZ.fromList
                |> Maybe.andThen (LZ.findFirst <| (==) presortCurrent)
                |> Deck

        Nothing ->
            deck


sortBy : (a -> comparable) -> Deck a -> Deck a
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


map : (a -> b) -> Deck a -> Deck b
map mapFn (Deck zip_) =
    zip_
        |> Maybe.map (LZ.map mapFn)
        |> Deck


mapCurrent : (a -> a) -> Deck a -> Deck a
mapCurrent mapFn (Deck zip_) =
    zip_
        |> Maybe.map (LZ.mapCurrent mapFn)
        |> Deck



-- ACCESSORS


current : Deck a -> Maybe a
current (Deck mbz_) =
    case mbz_ of
        Nothing ->
            Nothing

        Just zip ->
            Just (LZ.current zip)


before : Deck a -> Maybe (List a)
before (Deck zip_) =
    Maybe.map LZ.before zip_


after : Deck a -> Maybe (List a)
after (Deck zip_) =
    Maybe.map LZ.after zip_



-- SELECTORS


select : (a -> Bool) -> Deck a -> Deck a
select predicate (Deck mbz_) =
    case mbz_ of
        Nothing ->
            Deck Nothing

        Just zip ->
            zip
                |> LZ.findFirst predicate
                |> Deck


next : Deck a -> Deck a
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


previous : Deck a -> Deck a
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
