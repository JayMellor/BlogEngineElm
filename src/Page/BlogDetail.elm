module Page.BlogDetail exposing (Message, Model, init, pageTitle, update, view)

{-| View the blog post
-}

import Blog exposing (Blog, Content, blogContentDecoder)
import Containers exposing (showError)
import Html.Styled exposing (..)
import Http
import Markdown



-- MODEL


type Model
    = New
    | Edit
    | Preview (Blog Content)
    | Loading String
    | Loaded (Blog Content)
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
                , expect = Http.expectJson BlogResponse blogContentDecoder
                }
    in
    ( model, cmd )


pageTitle : Model -> String
pageTitle model =
    case model of
        Loaded blog ->
            let
                { title } =
                    Blog.details blog
            in
            title

        _ ->
            -- todo
            "todo"



-- UPDATE


type Message
    = BlogResponse (Result Http.Error (Blog Content))


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


showBlog : Blog Content -> Html Message
showBlog blog =
    let
        content =
            Blog.content blog

        { title } =
            Blog.details blog
    in
    div []
        [ h3 [] [ text title ]
        , Markdown.toHtml [] content |> Html.Styled.fromUnstyled
        ]
