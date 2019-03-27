module Main exposing (main)

import Browser
import Deck exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Zipper as LZ exposing (Zipper)
import Types exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { deck : Deck Card
    , workSurface : Deck ( Card, CardState )
    , deckSearchField : String
    , focus : Focus
    }


type alias Card =
    { id : Int, title : String, content : String }


type CardState
    = Normal
    | Editing


type Focus
    = OnDeck
    | OnWorkSurface


defaultModel =
    { deck = [ Card 0 "Test" "content", Card 2 "Second" "more stuff here and this one is longer" ] |> Deck.fromList
    , workSurface = [ ( Card 1 "Test 2" "content again", Normal ) ] |> Deck.fromList
    , deckSearchField = ""
    , focus = OnDeck
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, Cmd.none )



-- UPDATE


type Msg
    = NewCard
    | Edit
    | PullSelectedFromDeck
    | ReturnSelectedToDeck
    | SetFocus Focus
    | SelectCard Focus Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ workSurface, deck } as model) =
    case msg of
        PullSelectedFromDeck ->
            let
                ( newDeck, newWorkSurface ) =
                    Deck.move (\c -> ( c, Normal )) ( deck, workSurface )
            in
            ( { model | deck = newDeck, workSurface = newWorkSurface }, Cmd.none )

        ReturnSelectedToDeck ->
            let
                ( newWorkSurface, newDeck ) =
                    Deck.move Tuple.first ( workSurface, deck )

                maybeChangeFocus ( m, c ) =
                    if Deck.isEmpty m.workSurface then
                        ( { m | focus = OnDeck }, c )

                    else
                        ( m, c )
            in
            ( { model | deck = newDeck, workSurface = newWorkSurface }, Cmd.none )
                |> maybeChangeFocus

        SelectCard focus cardId ->
            case focus of
                OnDeck ->
                    ( { model | deck = Deck.select (\c -> cardId == c.id) deck, focus = focus }
                    , Cmd.none
                    )

                OnWorkSurface ->
                    ( { model | workSurface = Deck.select (\( c, _ ) -> cardId == c.id) workSurface, focus = focus }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { deck, workSurface, focus } =
    div [ id "app" ]
        [ viewDeck (focus == OnDeck) ( Deck.current deck, deck |> Deck.toList )
        , viewWorkSurface (focus == OnWorkSurface) ( Deck.current workSurface |> Maybe.map Tuple.first, workSurface |> Deck.toList )
        , button [ onClick PullSelectedFromDeck ] [ text "→" ]
        , button [ onClick ReturnSelectedToDeck ] [ text "←" ]
        ]


viewDeck : Bool -> ( Maybe Card, List Card ) -> Html Msg
viewDeck deckFocused ( currentCard_, cards ) =
    let
        viewFn c =
            case currentCard_ of
                Just currentCard ->
                    viewDeckCard (deckFocused && c.id == currentCard.id) c

                Nothing ->
                    viewDeckCard False c
    in
    div [ id "deck" ]
        (List.map viewFn cards)


viewWorkSurface : Bool -> ( Maybe Card, List ( Card, CardState ) ) -> Html Msg
viewWorkSurface workSurfaceFocused ( currentCard_, cards ) =
    let
        viewFn c =
            case currentCard_ of
                Just currentCard ->
                    viewNormalCard (workSurfaceFocused && c.id == currentCard.id) c

                Nothing ->
                    viewNormalCard False c
    in
    div [ id "work-surface" ]
        (cards
            |> List.map Tuple.first
            |> List.map viewFn
        )



-- CARD VIEWS


viewDeckCard : Bool -> Card -> Html Msg
viewDeckCard isCurrent ({ title, content } as card) =
    div
        [ id <| "card-" ++ String.fromInt card.id
        , classList [ ( "deck-card", True ), ( "current", isCurrent ) ]
        , onClick (SelectCard OnDeck card.id)
        ]
        [ h3 [] [ text title ]
        , div [] [ text content ]
        ]


viewNormalCard : Bool -> Card -> Html Msg
viewNormalCard isCurrent ({ title, content } as card) =
    div
        [ id <| "card-" ++ String.fromInt card.id
        , classList [ ( "card", True ), ( "current", isCurrent ) ]
        , onClick (SelectCard OnWorkSurface card.id)
        ]
        [ h3 [] [ text title ]
        , div [] [ text content ]
        ]


viewEditingCard : Card -> Html Msg
viewEditingCard card =
    viewDeckCard True card
