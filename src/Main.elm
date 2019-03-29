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
    { deck : Deck DeckCard
    , workSurface : Deck WorkCard
    , deckSearchField : String
    , focus : Focus
    }


type alias DeckCard =
    { id : Int, title : String, content : String, deckPosition : Int, workPosition : Maybe Int }


type alias WorkCard =
    { id : Int, title : String, content : String, deckPosition : Maybe Int, workPosition : Int, cardState : CardState }


deckToWorkCard : Int -> DeckCard -> WorkCard
deckToWorkCard maxWorkPosition c =
    { id = c.id, title = c.title, content = c.content, deckPosition = Just c.deckPosition, workPosition = c.workPosition |> Maybe.withDefault ((maxWorkPosition + maxInt) // 2), cardState = Normal }


workCardToDeck : Int -> WorkCard -> DeckCard
workCardToDeck maxDeckPosition c =
    { id = c.id, title = c.title, content = c.content, deckPosition = c.deckPosition |> Maybe.withDefault ((maxDeckPosition + maxInt) // 2), workPosition = Just c.workPosition }


type CardState
    = Normal
    | Editing


type Focus
    = OnDeck
    | OnWorkSurface


minInt =
    -2 ^ 31


maxInt =
    2 ^ 31 - 1


defaultModel =
    { deck =
        Deck.fromList
            [ DeckCard 0 "Test" "content" -1000 Nothing
            , DeckCard 2 "Second" "more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more." -500 Nothing
            , DeckCard 3 "Another test" "Let's have some content lorem ipsum." 0 Nothing
            , DeckCard 4 "YET Another test" "Let's have some content lorem ipsum." 500 Nothing
            , DeckCard 5 "Dolorum" "Let's have some content lorem ipsum." 1000 Nothing
            ]
    , workSurface =
        Deck.fromList
            [ WorkCard 1 "Test 2" "content again" (Just 250) 0 Normal
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
    | MoveUp
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

        MoveUp ->
            case focus of
                OnDeck ->
                    -- TODO : Moving API? Maybe:
                    -- if it's already the first card, break
                    -- if it's the second card, set it's position to: (minInt + first.pos) // 2
                    -- if it's any other card, set it's position to: (first.pos + sec.pos) // 2
                    -- Maybe a separate "Positions" module?
                    -- Should we also randomize within that interval (sha1 of title & id, for instance?)
                    ( model, Cmd.none )

                OnWorkSurface ->
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
                                |> Deck.sortBy .deckPosition

                        maxWorkPosition =
                            workSurface
                                |> Deck.toList
                                |> List.map .workPosition
                                |> List.maximum
                                |> Maybe.withDefault maxInt

                        newWorkSurface =
                            workSurface
                                |> Deck.insert (deckToWorkCard maxWorkPosition selected)
                                |> Deck.sortBy .workPosition
                    in
                    ( { model | deck = newDeck, workSurface = newWorkSurface, focus = OnWorkSurface }, Cmd.none )

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
                                |> List.map .deckPosition
                                |> List.maximum
                                |> Maybe.withDefault maxInt

                        newDeck =
                            deck
                                |> Deck.insert (workCardToDeck maxDeckPosition selected)
                                |> Deck.sortBy .deckPosition
                    in
                    ( { model | deck = newDeck, workSurface = newWorkSurface, focus = OnDeck }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetFocus newFocus ->
            ( { model | focus = newFocus }, Cmd.none )

        SelectCard newFocus cardId ->
            case newFocus of
                OnDeck ->
                    ( { model | deck = Deck.select (\c -> c.id == cardId) deck, focus = newFocus }
                    , Cmd.none
                    )

                OnWorkSurface ->
                    ( { model | workSurface = Deck.select (\c -> c.id == cardId) workSurface, focus = newFocus }
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

                ( "alt+up", _ ) ->
                    update MoveUp model

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { deck, workSurface, focus } =
    div [ id "app" ]
        [ viewDeck (focus == OnDeck) ( Deck.current deck, deck |> Deck.toList )
        , viewWorkSurface (focus == OnWorkSurface) ( Deck.current workSurface, workSurface |> Deck.toList )
        ]


viewDeck : Bool -> ( Maybe DeckCard, List DeckCard ) -> Html Msg
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


viewWorkSurface : Bool -> ( Maybe WorkCard, List WorkCard ) -> Html Msg
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


viewDeckCard : Bool -> DeckCard -> Html Msg
viewDeckCard isCurrent ({ title, content } as card) =
    div
        [ id <| "card-" ++ String.fromInt card.id
        , classList [ ( "deck-card", True ), ( "current", isCurrent ) ]
        , onClick (SelectCard OnDeck card.id)
        ]
        [ h3 [] [ text title ]
        , div [] [ text content ]
        ]


viewNormalCard : Bool -> WorkCard -> Html Msg
viewNormalCard isCurrent ({ title, content } as card) =
    div
        [ id <| "card-" ++ String.fromInt card.id
        , classList [ ( "card", True ), ( "current", isCurrent ) ]
        , onClick (SelectCard OnWorkSurface card.id)
        ]
        [ h3 [] [ text title ]
        , div [] [ text content ]
        ]


viewEditingCard : WorkCard -> Html Msg
viewEditingCard card =
    viewNormalCard True card



-- SUBSCRIPTIONS


port keyboard : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ keyboard Keyboard ]
