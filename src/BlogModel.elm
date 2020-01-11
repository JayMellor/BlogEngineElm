module BlogModel exposing (Blog, blogAuthor, blogListDecoder)

import Json.Decode as JD


type alias Blog =
    { id : String
    , author : Maybe Author
    , date : String
    , title : String
    , summary : String
    }


type alias Author =
    { id : String
    , username : String
    , firstName : Maybe String
    , surname : Maybe String
    }



-- DECODERS


blogListDecoder : JD.Decoder (List Blog)
blogListDecoder =
    JD.list blogDecoder


blogDecoder : JD.Decoder Blog
blogDecoder =
    JD.map5 Blog
        (JD.field "_id" JD.string)
        (JD.maybe (JD.field "author" authorDecoder))
        (JD.field "date" JD.string)
        (JD.field "title" JD.string)
        (JD.field "summary" JD.string)


authorDecoder : JD.Decoder Author
authorDecoder =
    JD.map4 Author
        (JD.field "_id" JD.string)
        (JD.field "username" JD.string)
        (JD.maybe (JD.field "firstName" JD.string))
        (JD.maybe (JD.field "surname" JD.string))



-- HELPERS


blogAuthor : Blog -> String
blogAuthor blog =
    case blog.author of
        Just author ->
            String.concat [ "by ", authorName author ]

        Nothing ->
            "Unknown Author"


authorName : Author -> String
authorName author =
    case author.firstName of
        Just firstName ->
            case author.surname of
                Just surname ->
                    String.concat [ firstName, " ", surname ]

                Nothing ->
                    firstName

        Nothing ->
            author.username
