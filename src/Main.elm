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
    { deck : Deck
    , workSurface : Deck
    , deckSearchField : String
    , mode : Mode
    }


swapCard : Int -> Card -> Card
swapCard maxPosition c =
    let
        newPosition =
            c.otherPosition |> Maybe.withDefault ((maxPosition + maxInt) // 2)
    in
    { id = c.id, title = c.title, content = c.content, position = newPosition, otherPosition = Just c.position }


type CardState
    = Normal
    | Editing


type Mode
    = OnDeck
    | OnWorkSurface CardState


defaultModel =
    { deck =
        Deck.fromList
            [ Card 0 "Test" "content" -1000 Nothing
            , Card 2 "Second" "more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more." -500 Nothing
            , Card 3 "Another test" "Let's have some content lorem ipsum." 0 Nothing
            , Card 4 "YET Another test" "Let's have some content lorem ipsum." 500 Nothing
            , Card 5 "Dolorum" "Let's have some content lorem ipsum." 1000 Nothing
            ]
    , workSurface =
        Deck.fromList
            [ Card 1 "Test 2" "content again" 0 (Just 250)
            ]
    , deckSearchField = ""
    , mode = OnDeck
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
    | MoveUp
    | MoveDown
    | PullSelectedFromDeck
    | ReturnSelectedToDeck
    | SetMode Mode
    | SelectCard Mode Int
    | Keyboard String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ workSurface, deck, mode } as model) =
    case msg of
        Next ->
            case mode of
                OnDeck ->
                    ( { model | deck = Deck.next deck }, Cmd.none )

                OnWorkSurface Normal ->
                    ( { model | workSurface = Deck.next workSurface }, Cmd.none )

                OnWorkSurface Editing ->
                    ( model, Cmd.none )

        Previous ->
            case mode of
                OnDeck ->
                    ( { model | deck = Deck.previous deck }, Cmd.none )

                OnWorkSurface Normal ->
                    ( { model | workSurface = Deck.previous workSurface }, Cmd.none )

                OnWorkSurface Editing ->
                    ( model, Cmd.none )

        MoveUp ->
            case mode of
                OnDeck ->
                    ( { model | deck = Deck.moveDelta -1 deck }, Cmd.none )

                OnWorkSurface Normal ->
                    ( { model | workSurface = Deck.moveDelta -1 workSurface }, Cmd.none )

                OnWorkSurface Editing ->
                    ( model, Cmd.none )

        MoveDown ->
            case mode of
                OnDeck ->
                    ( { model | deck = Deck.moveDelta 1 deck }, Cmd.none )

                OnWorkSurface Normal ->
                    ( { model | workSurface = Deck.moveDelta 1 workSurface }, Cmd.none )

                OnWorkSurface Editing ->
                    ( model, Cmd.none )

        PullSelectedFromDeck ->
            let
                selected_ =
                    Deck.current deck
            in
            case selected_ of
                Just selected ->
                    let
                        newDeck =
                            deck
                                |> Deck.remove
                                |> Deck.sortBy .position

                        maxWorkPosition =
                            workSurface
                                |> Deck.toList
                                |> List.map .position
                                |> List.maximum
                                |> Maybe.withDefault maxInt

                        newWorkSurface =
                            workSurface
                                |> Deck.insert (swapCard maxWorkPosition selected)
                                |> Deck.sortBy .position
                    in
                    ( { model | deck = newDeck, workSurface = newWorkSurface, mode = OnWorkSurface Normal }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ReturnSelectedToDeck ->
            let
                selected_ =
                    Deck.current workSurface
            in
            case selected_ of
                Just selected ->
                    let
                        newWorkSurface =
                            workSurface
                                |> Deck.remove

                        maxDeckPosition =
                            deck
                                |> Deck.toList
                                |> List.map .position
                                |> List.maximum
                                |> Maybe.withDefault maxInt

                        newDeck =
                            deck
                                |> Deck.insert (swapCard maxDeckPosition selected)
                                |> Deck.sortBy .position
                    in
                    ( { model | deck = newDeck, workSurface = newWorkSurface, mode = OnDeck }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetMode newMode ->
            ( { model | mode = newMode }, Cmd.none )

        SelectCard newMode cardId ->
            case newMode of
                OnDeck ->
                    ( { model | deck = Deck.select (\c -> c.id == cardId) deck, mode = newMode }
                    , Cmd.none
                    )

                OnWorkSurface Normal ->
                    ( { model | workSurface = Deck.select (\c -> c.id == cardId) workSurface, mode = newMode }
                    , Cmd.none
                    )

                OnWorkSurface Editing ->
                    ( model, Cmd.none )

        Keyboard keys ->
            case ( keys, mode ) of
                ( "left", OnWorkSurface Normal ) ->
                    update (SetMode OnDeck) model

                ( "right", OnDeck ) ->
                    update (SetMode (OnWorkSurface Normal)) model

                ( "up", _ ) ->
                    update Previous model

                ( "down", _ ) ->
                    update Next model

                ( "alt+left", OnWorkSurface Normal ) ->
                    update ReturnSelectedToDeck model

                ( "alt+right", OnDeck ) ->
                    update PullSelectedFromDeck model

                ( "alt+up", _ ) ->
                    update MoveUp model

                ( "alt+down", _ ) ->
                    update MoveDown model

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { deck, workSurface, mode } =
    div [ id "app" ]
        [ viewDeck (mode == OnDeck) ( Deck.current deck, deck |> Deck.toList )
        , viewWorkSurface (mode == OnWorkSurface Normal) ( Deck.current workSurface, workSurface |> Deck.toList )
        ]


viewDeck : Bool -> ( Maybe Card, List Card ) -> Html Msg
viewDeck deckFocused ( currentCard_, cards ) =
    let
        viewFn c =
            case currentCard_ of
                Just currentCard ->
                    ( c.id |> String.fromInt, viewCard (deckFocused && c.id == currentCard.id) c )

                Nothing ->
                    ( c.id |> String.fromInt, viewCard False c )
    in
    Keyed.node "div"
        [ id "deck" ]
        (List.map viewFn cards
            ++ [ ( "front", div [ class "deck-front" ] [] ) ]
        )


viewWorkSurface : Bool -> ( Maybe Card, List Card ) -> Html Msg
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
            |> List.map viewFn
        )



-- CARD VIEWS


viewCard : Bool -> Card -> Html Msg
viewCard isCurrent ({ title, content } as card) =
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
        , onClick (SelectCard (OnWorkSurface Normal) card.id)
        ]
        [ h3 [] [ text title ]
        , div [] [ text content ]
        ]


viewEditingCard : Card -> Html Msg
viewEditingCard card =
    viewNormalCard True card



-- SUBSCRIPTIONS


port keyboard : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ keyboard Keyboard ]
