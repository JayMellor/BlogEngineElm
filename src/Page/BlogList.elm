module Page.BlogList exposing (Message, Model, init, update, view)

{-| View a list of Blogs
-}

import Blog exposing (Blog, Summary, blogAuthor, blogListDecoder)
import Containers exposing (card, cardTitle, listContainer, showError, subtleHyperlink)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Http



-- MODEL


type alias Model =
    { blogs : List (Blog Summary)
    , status : Status
    }


type Status
    = Loading
    | Success (List (Blog Summary))
    | Failure Http.Error


init : () -> ( Model, Cmd Message )
init _ =
    let
        model =
            { blogs = []
            , status = Loading
            }
    in
    ( model, getBlogs )


getBlogs : Cmd Message
getBlogs =
    Http.get
        { url = "http://localhost:4080/api/blogs"
        , expect = Http.expectJson BlogResponse blogListDecoder
        }



-- UPDATE


type Message
    = BlogResponse (Result Http.Error (List (Blog Summary)))


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        BlogResponse response ->
            case response of
                Ok blogs ->
                    ( { model | blogs = blogs, status = Success blogs }, Cmd.none )

                Err msg ->
                    ( { model | blogs = [], status = Failure msg }, Cmd.none )



-- VIEW


view : Model -> Html Message
view model =
    div []
        (case model.status of
            Loading ->
                [ text "loading" ]

            Success blogs ->
                [ listBlogs blogs
                ]

            Failure error ->
                [ text "failed to get blogs - "
                , showError error
                ]
        )


listBlogs : List (Blog Summary) -> Html Message
listBlogs blogsList =
    div [ css [ listContainer ] ] (List.map blogCard blogsList)


blogCard : Blog Summary -> Html Message
blogCard blog =
    let
        { title } =
            Blog.details blog

        summaryText =
            Blog.summary blog
    in
    a [ css [ card, subtleHyperlink ], href (blogDetailLink blog) ]
        [ div [ css [ cardTitle ] ] [ text (String.concat [ title, " ", blogAuthor blog ]) ]
        , div [] [ text summaryText ]
        ]



-- todo convert to Tasks


blogDetailLink : Blog Summary -> String
blogDetailLink blog =
    let
        { id } =
            Blog.details blog
    in
    String.concat [ "/blogs/", id ]
