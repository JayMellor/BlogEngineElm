module Main exposing (Message(..), Model, Status(..), init, update)

import Browser
import Html exposing (Html, button, div, li, pre, text, ul)
import Http
import Json.Decode exposing (Decoder, field, int, maybe, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Blog =
    { id : String
    , author : Maybe String
    , date : String
    , title : String
    , summary : String
    }


type alias Model =
    { blogs : List Blog
    , status : Status
    }


type Status
    = Loading
    | Success (List Blog)
    | Failure Http.Error


init : () -> ( Model, Cmd Message )
init _ =
    ( Model [] Loading
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
        (maybe (field "author" string))
        (field "date" string)
        (field "title" string)
        (field "summary" string)



-- UPDATE


type Message
    = BlogResponse (Result Http.Error (List Blog))


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        BlogResponse response ->
            case response of
                Ok blogs ->
                    ( Model blogs (Success blogs), Cmd.none )

                Err msg ->
                    ( Model [] (Failure msg), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Message
view model =
    case model.status of
        Loading ->
            text "loading"

        Success blogs ->
            div []
                [ text "got the blogs"
                , listBlogs blogs
                ]

        Failure error ->
            div []
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
    ul [] (List.map mapBlog blogsList)


mapBlog : Blog -> Html Message
mapBlog blog =
    li []
        [ text (String.concat [ blog.title, ", ", blogAuthor blog, "; " ])
        , div [] [ text blog.summary ]
        ]


blogAuthor : Blog -> String
blogAuthor blog =
    case blog.author of
        Just author ->
            String.concat [ "by ", author ]

        Nothing ->
            "Unknown Author"


errorContainer errorMessage =
    div []
        [ text "Error with page - "
        , text errorMessage
        ]
