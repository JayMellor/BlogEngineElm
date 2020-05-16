module NavBar exposing (Message(..), Model, init, update, view)

{-| Navigation bar
-}

import ColorScheme exposing (navBarBackground, navBarText)
import Css exposing (..)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Route exposing (Route(..))



-- MODEL


type alias Model =
    { active : Route
    , authState : AuthState
    , appName : String
    }


type AuthState
    = NotLoggedIn
    | LoggedIn


init : String -> Model
init title =
    Model NotFound NotLoggedIn title



-- DISPLAYED LINKS


links : List DisplayedLink
links =
    [ { route = BlogListRoute
      , description = "Blogs"
      , link = "/blogs"
      }
    ]


type alias DisplayedLink =
    { route : Route
    , description : String
    , link : String
    }


type DisplayedLinks
    = NoneActive (List DisplayedLink)
    | OneActive (List DisplayedLink) DisplayedLink



-- UPDATE


type Message
    = RouteLoaded Route
    | LogInSuccess
    | LogOutSuccess


update : Message -> Model -> ( Model, Cmd Message )
update message navBar =
    case message of
        RouteLoaded route ->
            ( { navBar | active = route }
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
        , linkList active
        , div [] [ authCard authState ]
        ]


brightenActiveLink : Route -> DisplayedLink -> Html Message
brightenActiveLink activePage link =
    if activePage == link.route then
        div [] [text "active"]

    else
        div [] [text "not"]


linkList : Route -> Html Message
linkList active =
    div [] <|
        List.map (brightenActiveLink active) links


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
