module Containers exposing (card, cardTitle, errorContainer, listContainer, showError, subtleHyperlink)

{-| View containers
-}

import ColorScheme exposing (navBarBackground, textColor)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Http



-- ELEMENTS


showError : Http.Error -> Html a
showError httpError =
    case httpError of
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


errorContainer : String -> Html a
errorContainer errorMessage =
    div [ css [ errorView ] ]
        [ text "Error with page - "
        , text errorMessage
        ]


cardTitle : Style
cardTitle =
    batch
        [ marginBottom (Css.em 1)
        , fontWeight bold
        ]


card : Style
card =
    batch
        [ border3 (px 1) solid navBarBackground
        , margin (px 4)
        , padding (Css.em 1)
        , borderRadius (px 5)
        ]


listContainer : Style
listContainer =
    batch
        [ displayFlex
        , flexDirection column
        ]


subtleHyperlink : Style
subtleHyperlink =
    batch
        [ textDecoration none
        , color textColor
        ]



-- STYLES


errorView : Style
errorView =
    batch
        [ border (px 12)
        ]
