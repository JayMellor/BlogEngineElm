module Main exposing (main)

import BlogModel exposing (..)
import Browser
import Browser.Navigation as Nav
import ColorScheme exposing (..)
import Css exposing (..)
import Css.Global
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Http
import Url
import Url.Parser as Parser exposing ((</>))



-- MAIN


programName =
    "Blog Engine"


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
    { blogs : List Blog
    , status : Status
    , key : Nav.Key
    , currentRoute : Route
    }


type Status
    = Loading
    | Success (List Blog)
    | Failure Http.Error


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init flags url key =
    ( Model [] Loading key (url |> Url.toString |> toRoute)
    , Http.get
        { url = "http://localhost:4080/api/blogs"
        , expect = Http.expectJson BlogResponse blogListDecoder
        }
    )



-- UPDATE


type Message
    = BlogResponse (Result Http.Error (List Blog))
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        BlogResponse response ->
            case response of
                Ok blogs ->
                    ( { model | blogs = blogs, status = Success blogs }, Cmd.none )

                Err msg ->
                    ( { model | blogs = [], status = Failure msg }, Cmd.none )

        UrlChanged url ->
            ( { model | currentRoute = url |> Url.toString |> toRoute  }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


type Route
    = Blogs
    | Blog String
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Blogs (Parser.s "blogs")
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
subscriptions model =
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
                    , div [] [
                        text (case model.currentRoute of
                            NotFound -> 
                                "Not found"
                            Blogs -> 
                                "Blog List"
                            Blog blogId -> 
                                "Blog " ++ blogId)
                    ]
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
    div [ css [ bodyContainer ] ] (pageBodySelector model)


pageFooter : Model -> Html Message
pageFooter model =
    footer [ css [ footerContainer ] ]
        [ div [ css [ navBarItem ] ] [ text "By Jay Mellor" ] ]


pageBodySelector : Model -> List (Html Message)
pageBodySelector model =
    case model.status of
        Loading ->
            [ text "loading" ]

        Success blogs ->
            [ listBlogs blogs
            ]

        Failure error ->
            [ text "failed to get blogs - "
            , case error of
                Http.BadStatus status ->
                    errorContainer (String.fromInt status)

                Http.NetworkError ->
                    errorContainer "Error with network"

                Http.BadUrl url ->
                    errorContainer (String.concat [ "Error using URL: ", url ])

                Http.Timeout ->
                    errorContainer "Got nothing back"

                Http.BadBody message ->
                    errorContainer message
            ]


listBlogs : List Blog -> Html Message
listBlogs blogsList =
    div [ css [ listContainer ] ] (List.map blogCard blogsList)


blogCard : Blog -> Html Message
blogCard blog =
    a [ css [ card, subtleHyperlink ], href (blogDetailLink blog) ]
        [ div [ css [ cardTitle ] ] [ text (String.concat [ blog.title, " ", blogAuthor blog ]) ]
        , div [] [ text blog.summary ]
        ]


blogDetailLink blog =
    String.concat [ "/blog/", blog.id ]


errorContainer errorMessage =
    div [ css [ errorView ] ]
        [ text "Error with page - "
        , text errorMessage
        ]



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


listContainer : Style
listContainer =
    batch
        [ displayFlex
        , flexDirection column
        ]


errorView : Style
errorView =
    batch
        [ border (px 12)
        ]


card : Style
card =
    batch
        [ border3 (px 1) solid navBarBackground
        , margin (px 4)
        , padding (Css.em 1)
        , borderRadius (px 5)
        ]


subtleHyperlink : Style
subtleHyperlink =
    batch
        [ textDecoration none
        , color textColor
        ]


cardTitle : Style
cardTitle =
    batch
        [ marginBottom (Css.em 1)
        , fontWeight bold
        ]
