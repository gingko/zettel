port module Main exposing (main)

import Browser
import Deck exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import List.Zipper as LZ exposing (Zipper)
import Types exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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
    { deck =
        Deck.fromList
            [ Card 0 "Test" "content"
            , Card 2 "Second" "more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more."
            , Card 3 "Another test" "Let's have some content lorem ipsum."
            , Card 4 "YET Another test" "Let's have some content lorem ipsum."
            , Card 5 "Dolorum" "Let's have some content lorem ipsum."
            ]
    , workSurface =
        Deck.fromList
            [ ( Card 1 "Test 2" "content again", Normal )
            ]
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
    | Next
    | Previous
    | PullSelectedFromDeck
    | ReturnSelectedToDeck
    | SetFocus Focus
    | SelectCard Focus Int
    | Keyboard String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ workSurface, deck, focus } as model) =
    case msg of
        Next ->
            case focus of
                OnDeck ->
                    ( { model | deck = Deck.next deck }, Cmd.none )

                OnWorkSurface ->
                    ( { model | workSurface = Deck.next workSurface }, Cmd.none )

        Previous ->
            case focus of
                OnDeck ->
                    ( { model | deck = Deck.previous deck }, Cmd.none )

                OnWorkSurface ->
                    ( { model | workSurface = Deck.previous workSurface }, Cmd.none )

        PullSelectedFromDeck ->
            let
                ( newDeck, newWorkSurface ) =
                    Deck.move (\c -> ( c, Normal )) ( deck, workSurface )
            in
            ( { model | deck = newDeck, workSurface = newWorkSurface, focus = OnWorkSurface }, Cmd.none )

        ReturnSelectedToDeck ->
            let
                ( newWorkSurface, newDeck ) =
                    Deck.move Tuple.first ( workSurface, deck )
            in
            ( { model | deck = newDeck, workSurface = newWorkSurface, focus = OnDeck }, Cmd.none )

        SetFocus newFocus ->
            ( { model | focus = newFocus }, Cmd.none )

        SelectCard newFocus cardId ->
            case newFocus of
                OnDeck ->
                    ( { model | deck = Deck.select (\c -> cardId == c.id) deck, focus = newFocus }
                    , Cmd.none
                    )

                OnWorkSurface ->
                    ( { model | workSurface = Deck.select (\( c, _ ) -> cardId == c.id) workSurface, focus = newFocus }
                    , Cmd.none
                    )

        Keyboard keys ->
            case ( keys, focus ) of
                ( "left", OnWorkSurface ) ->
                    update (SetFocus OnDeck) model

                ( "right", OnDeck ) ->
                    update (SetFocus OnWorkSurface) model

                ( "up", _ ) ->
                    update Previous model

                ( "down", _ ) ->
                    update Next model

                ( "alt+left", OnWorkSurface ) ->
                    update ReturnSelectedToDeck model

                ( "alt+right", OnDeck ) ->
                    update PullSelectedFromDeck model

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { deck, workSurface, focus } =
    div [ id "app" ]
        [ viewDeck (focus == OnDeck) ( Deck.current deck, deck |> Deck.toList )
        , viewWorkSurface (focus == OnWorkSurface) ( Deck.current workSurface |> Maybe.map Tuple.first, workSurface |> Deck.toList )
        ]


viewDeck : Bool -> ( Maybe Card, List Card ) -> Html Msg
viewDeck deckFocused ( currentCard_, cards ) =
    let
        viewFn c =
            case currentCard_ of
                Just currentCard ->
                    ( c.id |> String.fromInt, viewDeckCard (deckFocused && c.id == currentCard.id) c )

                Nothing ->
                    ( c.id |> String.fromInt, viewDeckCard False c )
    in
    Keyed.node "div"
        [ id "deck" ]
        (List.map viewFn cards
            ++ [ ( "front", div [ class "deck-front" ] [] ) ]
        )


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



-- SUBSCRIPTIONS


port keyboard : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ keyboard Keyboard ]
