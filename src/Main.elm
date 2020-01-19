module Main exposing (main)

{-| Central module handling routing, messages and subscriptions
-}

import Browser
import Browser.Navigation as Nav
import ColorScheme exposing (..)
import Css exposing (..)
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Page exposing (Skeleton)
import Page.BlogDetail as BlogDetail
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


type Page
    = NotFoundPage
    | BlogListPage BlogList.Model
    | BlogDetailPage String BlogDetail.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Message )
init _ url key =
    let
        model =
            { key = key
            , currentRoute = toRoute url
            , currentPage = NotFoundPage
            }
    in
    initCurrentPage
        ( model
        , Cmd.none
        )


{-| Initialises the page as set by the route
-}
initCurrentPage : ( Model, Cmd Message ) -> ( Model, Cmd Message )
initCurrentPage ( model, existingCommands ) =
    let
        ( currentPage, mappedPageCommands ) =
            case model.currentRoute of
                Head ->
                    -- todo find better way of redirecting to Blogs
                    ( NotFoundPage, Nav.pushUrl model.key "/blogs" )

                BlogListRoute ->
                    -- todo find general way to convert messages etc if necessary
                    let
                        ( blogListModel, blogListCmds ) =
                            BlogList.init ()
                    in
                    ( BlogListPage blogListModel, Cmd.map BlogListMessageReceived blogListCmds )

                BlogDetailRoute blogId ->
                    let
                        ( blogDetailModel, blogDetailCmds ) =
                            BlogDetail.init blogId
                    in
                    ( BlogDetailPage blogId blogDetailModel
                    , Cmd.map BlogDetailMessageReceived blogDetailCmds
                    )

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
      -- Page messages
    | BlogListMessageReceived BlogList.Message
    | BlogDetailMessageReceived BlogDetail.Message


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case ( message, model.currentPage ) of
        -- General messages
        ( UrlChanged url, _ ) ->
            ( { model | currentRoute = toRoute url }, Cmd.none )
                |> initCurrentPage

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        -- Page-specific messages
        ( BlogListMessageReceived subMessage, BlogListPage blogListModel ) ->
            BlogList.update subMessage blogListModel
                |> updateUsing BlogListPage BlogListMessageReceived model

        ( BlogDetailMessageReceived subMessage, BlogDetailPage blogId blogDetailModel ) ->
            BlogDetail.update subMessage blogDetailModel
                |> updateUsing (BlogDetailPage blogId) BlogDetailMessageReceived model

        ( _, _ ) ->
            ( model, Cmd.none )


{-| Provide an updated model and command according to submodule's message.

    updateUsing SubPage SubMessageReceived model (SubModel.update SubMessage subModel)

    will update the submodule model with SubMessage while updating the model with SubMessageReceived

-}
updateUsing :
    (subModel -> Page)
    -> (subMessage -> Message)
    -> Model
    -> ( subModel, Cmd subMessage )
    -> ( Model, Cmd Message )
updateUsing page subMessage model ( subModel, subCmd ) =
    ( Model model.key model.currentRoute (page subModel)
    , Cmd.map subMessage subCmd
    )



-- ROUTES


type Route
    = Head
    | BlogListRoute
    | BlogDetailRoute String
    | NotFound


{-| Converts a URL to a route using the parser
-}
toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (Parser.parse routeParser url)


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Head Parser.top
        , Parser.map BlogListRoute (Parser.s "blogs")
        , Parser.map BlogDetailRoute (Parser.s "blogs" </> Parser.string)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Message
view model =
    let
        { title, content } =
            pageSelector model
    in
    { title = title
    , body =
        [ toUnstyled
            (styledPage []
                [ div [ css [ mainContentContainer ] ]
                    [ navBar model
                    , content
                    ]
                , pageFooter model
                ]
            )
        ]
    }


styledPage : List (Attribute Message) -> List (Html Message) -> Html Message
styledPage attributes children =
    styled div
        []
        attributes
        (globalStyleNode :: children)


{-| Sets gobal styles to e.g. the html and body nodes of the DOM
-}
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


navBar : Model -> Html Message
navBar model =
    div [ css [ navBarContainer ] ]
        [ div [ css [ navBarItem ] ] [ text programName ]
        ]


pageBody : Skeleton Message -> Skeleton Message
pageBody skeleton =
    { skeleton | content = div [ css [ bodyContainer ] ] [ skeleton.content ] }


pageFooter : Model -> Html Message
pageFooter model =
    footer [ css [ footerContainer ] ]
        [ div [ css [ navBarItem ] ] [ text "By Jay Mellor" ] ]


pageSelector : Model -> Skeleton Message
pageSelector model =
    case model.currentPage of
        NotFoundPage ->
            { title = "Not Found", content = div [] [ text "not found" ] } |> pageBody

        BlogListPage blogListModel ->
            { title = "Blog List", content = BlogList.view blogListModel |> Html.Styled.map BlogListMessageReceived } |> pageBody

        BlogDetailPage blogId blogDetailModel ->
            { title = "Blog " ++ blogId, content = BlogDetail.view blogDetailModel |> Html.Styled.map BlogDetailMessageReceived } |> pageBody



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
