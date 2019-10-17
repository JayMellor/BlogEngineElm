import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main = 
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = Int

init = 
  0


-- UPDATE

type Msg = Increment | Decrement | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of 
    Increment -> 
      model + 1
    
    Decrement -> 
      model - 1

    Reset ->
      0



-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    button [onClick Increment] [text "Increment me!"]
    , div [] [text (String.fromInt model)]
    , button [onClick Decrement] [text "Decrement me!"] 
    , button [onClick Reset] [text "Reset"]
  ]