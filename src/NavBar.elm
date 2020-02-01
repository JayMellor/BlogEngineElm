module NavBar exposing (Message(..), Model, init, update, view)

{-| Navigation bar
-}

import ColorScheme exposing (navBarBackground, navBarText)
import Css exposing (..)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Page exposing (Page(..))



-- MODEL


type alias Model =
    { active : Page
    , authState : AuthState
    , appName : String
    }


type AuthState
    = NotLoggedIn
    | LoggedIn


init : String -> Model
init title =
    Model Page.NotFoundPage NotLoggedIn title



-- UPDATE


type Message
    = PageLoaded Page
    | LogInSuccess
    | LogOutSuccess


update : Message -> Model -> ( Model, Cmd Message )
update message navBar =
    case message of
        PageLoaded page ->
            ( { navBar | active = page }
            , Cmd.none
            )

        LogInSuccess ->
            ( { navBar | authState = LoggedIn }, Cmd.none )

        LogOutSuccess ->
            ( { navBar | authState = NotLoggedIn }, Cmd.none )



-- VIEW


view : Model -> Html Message
view { active, authState, appName } =
    div [ css [ navBarContainer ] ]
        [ div [ css [ navBarItem ] ] [ text appName ]
        , div [] [ authCard authState ]
        ]


authCard : AuthState -> Html Message
authCard state =
    case state of
        NotLoggedIn ->
            div [] [ text "Not logged in" ]

        LoggedIn ->
            div [] [ text "You shouldn't be able to see this yet" ]



-- STYLES


navBarContainer : Style
navBarContainer =
    batch
        [ backgroundColor navBarBackground
        , color navBarText
        , Css.height (Css.em 3)
        , displayFlex
        , alignItems center
        ]


navBarItem : Style
navBarItem =
    batch
        [ padding (Css.rem 1)
        , fontSize large
        ]
