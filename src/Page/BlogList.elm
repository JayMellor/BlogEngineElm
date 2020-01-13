module Page.BlogList exposing (Message, Model, init, update, view)

{-| View a list of Blogs
-}

import BlogModel exposing (Blog, blogAuthor, blogListDecoder)
import Containers exposing (card, cardTitle, listContainer, showError, subtleHyperlink)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Http



-- MODEL


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
    = BlogResponse (Result Http.Error (List Blog))


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
-- todo move to below signature if necessary
-- view : Model -> { title : String, content : Html Message }


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


listBlogs : List Blog -> Html Message
listBlogs blogsList =
    div [ css [ listContainer ] ] (List.map blogCard blogsList)


blogCard : Blog -> Html Message
blogCard blog =
    a [ css [ card, subtleHyperlink ], href (blogDetailLink blog) ]
        [ div [ css [ cardTitle ] ] [ text (String.concat [ blog.title, " ", blogAuthor blog ]) ]
        , div [] [ text blog.summary ]
        ]


blogDetailLink : Blog -> String
blogDetailLink blog =
    String.concat [ "/blog/", blog.id ]
