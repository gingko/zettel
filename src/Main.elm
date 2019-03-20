module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Zipper exposing (..)


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
    { deck = Nothing
    , workSurface = Nothing
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
viewDeck isFocused deck =
    div [ id "deck" ] [ text "DECK" ]


viewWorkSurface : Bool -> Maybe (Zipper ( Card, CardState )) -> Html Msg
viewWorkSurface isFocused workSurface =
    div [ id "work-surface" ] [ text "WORKSURFACE" ]
