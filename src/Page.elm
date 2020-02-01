module Page exposing (Page(..), Skeleton)

import Html.Styled exposing (Html)
import Page.BlogDetail as BlogDetail
import Page.BlogList as BlogList


{-| Page model
-}



-- MODEL


type Page
    = NotFoundPage
    | BlogListPage BlogList.Model
    | BlogDetailPage String BlogDetail.Model


type alias Skeleton message =
    { title : String
    , content : Html message
    }
