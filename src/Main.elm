module Main exposing (main)

import Browser
import Deck exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    = Deck
    | WorkSurface


defaultModel =
    { deck = [ Card 0 "Test" "content", Card 2 "Second" "more stuff here and this one is longer" ] |> Deck.fromList
    , workSurface = [ ( Card 1 "Test 2" "content again", Normal ) ] |> Deck.fromList
    , deckSearchField = ""
    , focus = WorkSurface
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, Cmd.none )



-- UPDATE


type Msg
    = NewCard
    | Edit
    | PullFromDeck
    | AddToDeck
    | SetFocus Focus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "model" model
    in
    div [ id "app" ]
        [ viewDeck (model.focus == Deck) (model.deck |> Deck.toList)
        , viewWorkSurface (model.focus == WorkSurface) (model.workSurface |> Deck.toList)
        ]


viewDeck : Bool -> List Card -> Html Msg
viewDeck isFocused cards =
    div [ id "deck" ]
        (List.map viewDeckCard cards)


viewWorkSurface : Bool -> List ( Card, CardState ) -> Html Msg
viewWorkSurface isFocused cards =
    div [ id "work-surface" ]
        (cards
            |> List.map Tuple.first
            |> List.map viewNormalCard
        )



-- CARD VIEWS


viewDeckCard : Card -> Html Msg
viewDeckCard ({ title, content } as card) =
    div [ id <| "card-" ++ String.fromInt card.id, class "deck-card" ]
        [ h3 [] [ text title ]
        , div [] [ text content ]
        ]


viewNormalCard : Card -> Html Msg
viewNormalCard ({ title, content } as card) =
    div [ id <| "card-" ++ String.fromInt card.id, class "card" ]
        [ h3 [] [ text title ]
        , div [] [ text content ]
        ]


viewEditingCard : Card -> Html Msg
viewEditingCard card =
    viewDeckCard card
