module Deck exposing (Deck, current, fromList, insert, isEmpty, next, previous, remove, select, sort, sortBy, toList)

import List.Zipper as LZ exposing (Zipper(..))
import Types exposing (..)


type Deck a
    = Deck (Maybe (Zipper a))


insert : a -> Deck a -> Deck a
insert newCurrent ((Deck zip_) as deck) =
    case zip_ of
        Just zip ->
            (newCurrent :: LZ.toList zip)
                |> LZ.fromList
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


current : Deck a -> Maybe a
current (Deck mbz_) =
    case mbz_ of
        Nothing ->
            Nothing

        Just zip ->
            Just (LZ.current zip)


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
