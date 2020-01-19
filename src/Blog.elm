module Blog exposing (Blog, BlogDetails, Content, Summary, blogAuthor, blogContentDecoder, blogListDecoder, content, details, summary)

import Author exposing (Author)
import Json.Decode as JD


type Blog extra
    = Blog BlogDetails extra


type alias BlogDetails =
    { id : String
    , author : Maybe Author
    , date : String
    , title : String
    }


type Summary
    = Summary String


type Content
    = Content String



-- EXTRACTORS


details : Blog a -> BlogDetails
details (Blog blogDetails _) =
    blogDetails


summary : Blog Summary -> String
summary (Blog _ (Summary text)) =
    text


content : Blog Content -> String
content (Blog _ (Content text)) =
    text



-- DECODERS


blogDetailsDecoder : JD.Decoder BlogDetails
blogDetailsDecoder =
    JD.map4 BlogDetails
        (JD.field "_id" JD.string)
        (JD.maybe (JD.field "author" Author.decoder))
        (JD.field "date" JD.string)
        (JD.field "title" JD.string)


blogListDecoder : JD.Decoder (List (Blog Summary))
blogListDecoder =
    JD.list blogSummaryDecoder


partialBlogDecoder : JD.Decoder a -> JD.Decoder (Blog a)
partialBlogDecoder =
    JD.map2 Blog
        blogDetailsDecoder


blogSummaryDecoder : JD.Decoder (Blog Summary)
blogSummaryDecoder =
    partialBlogDecoder summaryDecoder


summaryDecoder : JD.Decoder Summary
summaryDecoder =
    JD.map Summary
        (JD.field "summary" JD.string)


blogContentDecoder : JD.Decoder (Blog Content)
blogContentDecoder =
    partialBlogDecoder
        contentDecoder


contentDecoder : JD.Decoder Content
contentDecoder =
    JD.map Content
        (JD.field "content" JD.string)


blogAuthor : Blog a -> String
blogAuthor (Blog blogDetails _) =
    case blogDetails.author of
        Just author ->
            String.concat [ "by ", Author.name author ]

        Nothing ->
            "by Unknown Author"
