module Page.BlogDetail exposing (Message, Model, init, update, view)

{-| View the blog post
-}

import BlogModel exposing (Blog, blogDecoder)
import Containers exposing (showError)
import Html.Styled exposing (..)
import Http



-- MODEL


type Model
    = New
    | Display
    | Edit
    | Loading String
    | Loaded Blog
    | Failed Http.Error
    | Saving
    | Saved


init : String -> ( Model, Cmd Message )
init blogId =
    let
        model =
            Loading blogId

        cmd =
            Http.get
                { url = "http://localhost:4080/api/blogs/" ++ blogId
                , expect = Http.expectJson BlogResponse blogDecoder
                }
    in
    ( model, cmd )



-- UPDATE


type Message
    = BlogResponse (Result Http.Error Blog)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        BlogResponse response ->
            case response of
                Ok blog ->
                    ( Loaded blog, Cmd.none )

                Err error ->
                    ( Failed error, Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    case model of
        Loading blogId ->
            div [] [ text ("Loading Blog" ++ blogId) ]

        Loaded blog ->
            showBlog blog

        Failed error ->
            showError error

        _ ->
            text "Not implemented yet"


showBlog : Blog -> Html Message
showBlog blog =
    div []
        [ h3 [] [ text blog.title ]
        , case blog.content of
            Just content ->
                p [] [ text content ]

            Nothing ->
                p [] [ text "No content" ]
        ]
