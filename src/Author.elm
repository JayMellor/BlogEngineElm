module Author exposing (Author, decoder, name)

{-| Model for the Author, which may appear against a blog
-}

import Json.Decode as JD



-- MODEL


type alias Author =
    { id : String
    , username : String
    , firstName : Maybe String
    , surname : Maybe String
    }



-- EXTRACTORS


name : Author -> String
name author =
    case author.firstName of
        Just firstName ->
            case author.surname of
                Just surname ->
                    String.concat [ firstName, " ", surname ]

                Nothing ->
                    firstName

        Nothing ->
            author.username



-- DECODERS


decoder : JD.Decoder Author
decoder =
    JD.map4 Author
        (JD.field "_id" JD.string)
        (JD.field "username" JD.string)
        (JD.maybe (JD.field "firstName" JD.string))
        (JD.maybe (JD.field "surname" JD.string))
