module Page exposing (Skeleton)

import Html.Styled exposing (Html)



-- MODEL


type alias Skeleton message =
    { title : String
    , content : Html message
    }
