module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Zipper as LZ exposing (Zipper)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { deck : Maybe (Zipper Card)
    , workSurface : Maybe (Zipper ( Card, CardState ))
    , deckSearchField : String
    , focus : Focus
    }


type CardState
    = Normal
    | Editing


type alias Card =
    { id : Int, title : String, content : String }


type Focus
    = Deck
    | WorkSurface


defaultModel =
    { deck = [ Card 0 "Test" "content" ] |> LZ.fromList
    , workSurface = [ ( Card 1 "Test 2" "content again", Normal ) ] |> LZ.fromList
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
    div [ id "app" ]
        [ viewDeck (model.focus == Deck) model.deck
        , viewWorkSurface (model.focus == WorkSurface) model.workSurface
        ]


viewDeck : Bool -> Maybe (Zipper Card) -> Html Msg
viewDeck isFocused deck_ =
    ul [ id "deck" ]
        (case deck_ of
            Nothing ->
                []

            Just deck ->
                List.map viewDeckCard <| LZ.toList <| deck
        )


viewWorkSurface : Bool -> Maybe (Zipper ( Card, CardState )) -> Html Msg
viewWorkSurface isFocused workSurface_ =
    ul [ id "work-surface" ]
        (case workSurface_ of
            Nothing ->
                []

            Just workSurface ->
                List.map viewDeckCard <| List.map Tuple.first <| LZ.toList <| workSurface
        )



-- CARD VIEWS


viewDeckCard : Card -> Html Msg
viewDeckCard ({ title, content } as card) =
    li [ id <| "card-" ++ String.fromInt card.id ]
        [ h3 [] [ text title ]
        , div [] [ text content ]
        ]


viewNormalCard : Card -> Html Msg
viewNormalCard card =
    viewDeckCard card


viewEditingCard : Card -> Html Msg
viewEditingCard card =
    viewDeckCard card
