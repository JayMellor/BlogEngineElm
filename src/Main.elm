module Main exposing (main)

import BlogModel exposing (Blog)
import Browser
import Browser.Navigation as Nav
import ColorScheme exposing (..)
import Css exposing (..)
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Page.BlogList as BlogList
import Url
import Url.Parser as Parser exposing ((</>))



-- MAIN


programName : String
programName =
    "Blog Engine"


main : Program () Model Message
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , currentRoute : Route
    , currentPage : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init _ url key =
    let
        model =
            { key = key
            , currentRoute = url |> Url.toString |> toRoute
            , currentPage = NotFoundPage
            }
    in
    initCurrentPage
        ( model
        , Cmd.none
        )


type Page
    = NotFoundPage
    | BlogList BlogList.Model
    | ShowBlog String



-- Initalise the page from the route. Initialise a module relating to the page
-- todo needed?


initCurrentPage : ( Model, Cmd Message ) -> ( Model, Cmd Message )
initCurrentPage ( model, existingCommands ) =
    let
        ( currentPage, mappedPageCommands ) =
            case model.currentRoute of
                Head ->
                    let
                        ( blogModel, blogCmds ) =
                            BlogList.init ()
                    in
                    ( BlogList blogModel, Cmd.map BlogListMessageReceived blogCmds )

                Blogs ->
                    let
                        ( blogModel, blogCmds ) =
                            BlogList.init ()
                    in
                    ( BlogList blogModel, Cmd.map BlogListMessageReceived blogCmds )

                Blog blogId ->
                    ( ShowBlog blogId, Cmd.none )

                NotFound ->
                    ( NotFoundPage, Cmd.none )
    in
    ( { model | currentPage = currentPage }
    , Cmd.batch [ existingCommands, mappedPageCommands ]
    )



-- UPDATE


type Message
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | BlogListMessageReceived BlogList.Message


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case ( message, model.currentPage ) of
        ( UrlChanged url, _ ) ->
            ( { model | currentRoute = url |> Url.toString |> toRoute }, Cmd.none )
                |> initCurrentPage

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( BlogListMessageReceived subMessage, BlogList blogListModel ) ->
            BlogList.update subMessage blogListModel 
                |> updateTo BlogList BlogListMessageReceived model

        ( _, _ ) ->
            ( model, Cmd.none )

-- todo simplify below?
updateTo : (subModel -> Page) 
    -> (subMessage -> Message) 
    -> Model
    -> (subModel, Cmd subMessage)
    -> (Model, Cmd Message)
updateTo toPage toMessage model (subModel, subCmd) =
    (Model model.key model.currentRoute (toPage subModel)
    , Cmd.map toMessage subCmd)

-- remove Head?
type Route
    = Head
    | Blogs
    | Blog String
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Head Parser.top
        , Parser.map Blogs (Parser.s "blogs")
        , Parser.map Blog (Parser.s "blog" </> Parser.string)
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (Parser.parse routeParser url)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Message
view model =
    { title = programName
    , body =
        [ toUnstyled
            (page []
                [ div [ css [ mainContentContainer ] ]
                    [ navBar model
                    , pageBody model
                    ]
                , pageFooter model
                ]
            )
        ]
    }


globalStyleNode : Html Message
globalStyleNode =
    Css.Global.global
        [ Css.Global.html
            [ backgroundColor background
            , fontFamilies [ "Verdana", "Arial" ]
            , Css.height (pct 100)
            ]
        , Css.Global.body
            [ margin (px 0)
            , border (px 0)
            , Css.height (pct 100)
            ]
        , Css.Global.class "unstyled"
            [ -- Unstyled node wrapping the content in toUnstyled
              Css.height (pct 100)
            ]
        ]


page : List (Attribute Message) -> List (Html Message) -> Html Message
page attributes children =
    styled div
        []
        attributes
        (globalStyleNode :: children)


navBar : Model -> Html Message
navBar model =
    div [ css [ navBarContainer ] ]
        [ div [ css [ navBarItem ] ] [ text programName ]
        ]


pageBody : Model -> Html Message
pageBody model =
    div [ css [ bodyContainer ] ] [ pageBodySelector model ]


pageFooter : Model -> Html Message
pageFooter model =
    footer [ css [ footerContainer ] ]
        [ div [ css [ navBarItem ] ] [ text "By Jay Mellor" ] ]


pageBodySelector : Model -> Html Message
pageBodySelector model =
    case model.currentPage of
        NotFoundPage ->
            div [] [ text "not found" ]

        BlogList blogListModel ->
            BlogList.view blogListModel |> Html.Styled.map BlogListMessageReceived

        ShowBlog _ ->
            div [] [ text "Not implemented yet" ]



-- STYLES


mainContentContainer : Style
mainContentContainer =
    batch
        [ Css.height (pct 100)
        , Css.minHeight (pct 100)
        ]


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
        [ padding (Css.em 1)
        , fontSize large
        ]


bodyContainer : Style
bodyContainer =
    batch
        [ padding (Css.em 2)
        ]


footerContainer : Style
footerContainer =
    batch
        [ navBarContainer
        , flexDirection rowReverse
        ]
