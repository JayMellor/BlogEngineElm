module Main exposing (Model, Msg(..), init, main, update)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String }


init : Model
init =
    Model ""



-- UPDATE


type Msg
    = ChangeLeft String
    | ChangeRight String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeLeft newInput ->
            { model | input = newInput }

        ChangeRight newInput ->
            { model | input = inputToC newInput }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.input, onInput ChangeLeft ] []
        , text "˚C = "
        , input [ value (inputToF model.input), onInput ChangeRight ] []
        , text "˚F"
        ]


inputToF input =
    case String.toFloat input of
        Just number ->
            String.fromFloat (number * 1.8 + 32)

        Nothing ->
            ""


inputToC input =
    case String.toFloat input of
        Just number ->
            String.fromFloat ((number - 32) / 1.8)

        Nothing ->
            ""
