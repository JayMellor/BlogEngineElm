module Route exposing (Route(..), routeParser, toRoute)

{-| Route model for mapping possible URL routes of the application
-}

import Url
import Url.Parser as Parser exposing ((</>))


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
