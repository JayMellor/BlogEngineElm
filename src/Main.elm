module Main exposing (Message(..), Model, Status(..), init, update)

import Browser
import Browser.Navigation
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, maybe, string)
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


type alias Blog =
    { id : String
    , author : Maybe Author
    , date : String
    , title : String
    , summary : String
    }


type alias Author =
    { id : String
    , username : String
    , firstName : Maybe String
    , surname : Maybe String
    }


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


blogListDecoder : Decoder (List Blog)
blogListDecoder =
    Json.Decode.list blogDecoder


blogDecoder : Decoder Blog
blogDecoder =
    Json.Decode.map5 Blog
        (field "_id" string)
        (maybe (field "author" authorDecoder))
        (field "date" string)
        (field "title" string)
        (field "summary" string)


authorDecoder : Decoder Author
authorDecoder =
    Json.Decode.map4 Author
        (field "_id" string)
        (field "username" string)
        (maybe (field "firstName" string))
        (maybe (field "surname" string))


blogAuthor : Blog -> String
blogAuthor blog =
    case blog.author of
        Just author ->
            String.concat [ "by ", authorName author ]

        Nothing ->
            "Unknown Author"


authorName : Author -> String
authorName author =
    case author.firstName of
        Just firstName ->
            case author.surname of
                Just surname ->
                    String.concat [ firstName, " ", surname ]

                Nothing ->
                    firstName

        Nothing ->
            author.username



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
    , body = [ toUnstyled (pageBody model) ]
    }


pageBody : Model -> Html Message
pageBody model =
    div [ css [ page ] ]
        (case model.status of
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
        )


listBlogs : List Blog -> Html Message
listBlogs blogsList =
    div [ css [ listContainer ] ] (List.map blogCard blogsList)


blogCard : Blog -> Html Message
blogCard blog =
    div [ css [ card ] ]
        [ text (String.concat [ blog.title, ", ", blogAuthor blog, "; " ])
        , div [] [ text blog.summary ]
        ]


errorContainer errorMessage =
    div [ css [ errorView ] ]
        [ text "Error with page - "
        , text errorMessage
        ]



-- STYLES


page : Style
page =
    batch
        [ backgroundColor (rgb 250 250 3),
        Css.height (pct 100)
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
        [ backgroundColor (rgb 250 4 250)
        , margin (px 10)
        ]
