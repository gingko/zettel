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
    { id : Int, title : String, content : String, position : Int, otherPosition : Maybe Int }


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
            [ Card 0 "Test" "content" -1000 Nothing
            , Card 2 "Second" "more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more.more stuff here and this one is longer. More more." -500 Nothing
            , Card 3 "Another test" "Let's have some content lorem ipsum." 0 Nothing
            , Card 4 "YET Another test" "Let's have some content lorem ipsum." 500 Nothing
            , Card 5 "Dolorum" "Let's have some content lorem ipsum." 1000 Nothing
            ]
    , workSurface =
        Deck.fromList
            [ ( Card 1 "Test 2" "content again" 0 (Just 250), Normal )
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
                    case Maybe.map List.reverse <| Deck.before deck of
                        Just [] ->
                            ( model, Cmd.none )

                        Just [ prevCard ] ->
                            let
                                newDeck =
                                    deck
                                        |> Deck.mapCurrent (\c -> { c | position = (minInt + prevCard.position) // 2 })
                                        |> Deck.sortBy .position
                            in
                            ( { model | deck = newDeck }, Cmd.none )

                        Just (prevCard :: prevPrevCard :: _) ->
                            let
                                newDeck =
                                    deck
                                        |> Deck.mapCurrent (\c -> { c | position = (prevCard.position + prevPrevCard.position) // 2 })
                                        |> Deck.sortBy .position
                            in
                            ( { model | deck = newDeck }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                OnWorkSurface ->
                    case Maybe.map List.reverse <| Deck.before workSurface of
                        Just [] ->
                            ( model, Cmd.none )

                        Just [ ( prevCard, _ ) ] ->
                            let
                                newWorkSurface =
                                    workSurface
                                        |> Deck.mapCurrent (\( c, cs ) -> ( { c | position = (minInt + prevCard.position) // 2 }, cs ))
                                        |> Deck.sortBy (Tuple.first >> .position)
                            in
                            ( { model | workSurface = newWorkSurface }, Cmd.none )

                        Just (( prevCard, _ ) :: ( prevPrevCard, _ ) :: _) ->
                            let
                                newWorkSurface =
                                    workSurface
                                        |> Deck.mapCurrent (\( c, cs ) -> ( { c | position = (prevCard.position + prevPrevCard.position) // 2 }, cs ))
                                        |> Deck.sortBy (Tuple.first >> .position)
                            in
                            ( { model | workSurface = newWorkSurface }, Cmd.none )

                        Nothing ->
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
                                |> List.map (Tuple.first >> .position)
                                |> List.maximum
                                |> Maybe.withDefault maxInt

                        newWorkSurface =
                            workSurface
                                |> Deck.insert ( swapCard maxWorkPosition selected, Normal )
                                |> Deck.sortBy (Tuple.first >> .position)
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
                Just ( selected, _ ) ->
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
                    ( { model | workSurface = Deck.select (\( c, _ ) -> c.id == cardId) workSurface, focus = newFocus }
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


viewWorkSurface : Bool -> ( Maybe ( Card, CardState ), List ( Card, CardState ) ) -> Html Msg
viewWorkSurface workSurfaceFocused ( currentCard_, cards ) =
    let
        viewFn ( c, cs ) =
            case currentCard_ of
                Just ( currentCard, _ ) ->
                    viewNormalCard (workSurfaceFocused && c.id == currentCard.id) ( c, cs )

                Nothing ->
                    viewNormalCard False ( c, cs )
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


viewNormalCard : Bool -> ( Card, CardState ) -> Html Msg
viewNormalCard isCurrent ( { title, content } as card, cardState ) =
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
    viewNormalCard True ( card, Editing )



-- SUBSCRIPTIONS


port keyboard : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ keyboard Keyboard ]
