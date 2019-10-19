module Main exposing (main)

import BlogModel exposing (..)
import Browser
import Browser.Navigation
import ColorScheme exposing (..)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Http
import Url



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
    , key : Browser.Navigation.Key
    , url : Url.Url
    }


type Status
    = Loading
    | Success (List Blog)
    | Failure Http.Error


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Message )
init flags url key =
    ( Model [] Loading key url
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
            ( { model | url = url }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Message
view model =
    { title = programName
    , body = [ toUnstyled (page model) ]
    }


page : Model -> Html Message
page model =
    div [ css [ pageContainer] ]
        [ navBar model
        , pageBody model
        ]


navBar : Model -> Html Message
navBar model =
    div [ css [ navBarContainer ] ]
        [ div [css [navBarItem]] [text programName]
        ]


pageBody : Model -> Html Message
pageBody model =
    div [ css [ bodyContainer ] ] (pageBodySelector model)


pageBodySelector : Model -> List (Html Message)
pageBodySelector model =
    case model.status of
        Loading ->
            [ text "loading" ]

        Success blogs ->
            [ h1 [] [ text programName ]
            , listBlogs blogs
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
        [ div [ css [ cardTitle ] ] [ text (String.concat [ blog.title, ", ", blogAuthor blog, "; " ]) ]
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

pageContainer : Style 
pageContainer =
    batch   
        [
            fontFamilies ["Verdana", "Arial"]
        ]


navBarContainer : Style
navBarContainer =
    batch
        [ backgroundColor background
        , color navBarText
        , Css.height (Css.em 3)
        , displayFlex
        , alignItems center
        ]

navBarItem : Style
navBarItem =
    batch 
        [
            padding (Css.em 1)
            , fontSize large
        ]


bodyContainer : Style
bodyContainer =
    batch
        [ margin (Css.em 2)
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
        [ border3 (px 1) solid background
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
        ]
