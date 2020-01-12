module Page.BlogDetail exposing (..)

{-| View the blog post
-}

-- MODEL


type alias Model =
    { key1 : String
    }


init : () -> ( Model, Cmd Message )
init _ =
    let
        model =
            { key1 = "not implemented yet"
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Message
    = String


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( model, Cmd.none )



-- VIEW
