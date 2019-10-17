module Main exposing (Model, Msg(..), helperText, passwordLength, passwordsMatch, update, validityColor, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if not (passwordsMatch model.password model.passwordAgain) then
        helperText False "Passwords do not match"

    else if not (passwordLength model.password) then
        helperText False "Password is too short"

    else
        helperText True "OK"


passwordsMatch : String -> String -> Bool
passwordsMatch string1 string2 =
    string1 == string2


passwordLength : String -> Bool
passwordLength password =
    String.length password >= 8


helperText : Bool -> String -> Html msg
helperText isValid message =
    div [ style "color" (validityColor isValid) ] [ text message ]


validityColor : Bool -> String
validityColor isValid =
    if isValid then
        "green"

    else
        "red"
