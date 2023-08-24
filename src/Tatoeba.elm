module Tatoeba exposing
    ( Language(..)
    , Sentence
    , Text
    , Audio
    , random10
    , downloadAudio
    )


import Http
import Json.Decode exposing (Decoder)
import Url.Builder


import Audio exposing (AudioCmd)


type Language
    = English
    | Esperanto
    | Japanese
    | Portuguese
    | Spanish


languageToString : Language -> String
languageToString lang =
    case lang of
        English ->
            "eng"

        Esperanto ->
            "epo"

        Japanese ->
            "jpn"

        Portuguese ->
            "por"

        Spanish ->
            "spa"


urlForRandom10 : Language -> Language -> String
urlForRandom10 src dest =
    Url.Builder.crossOrigin
        "https://tatoeba.org"
        [ "en", "api_v0", "search" ]
        <| List.map ( \ ( paramName, value ) ->  Url.Builder.string paramName value )
            [ ( "from", Language.toString src )
            , ( "to", Language.toString dest )
            , ( "has_audio", "yes" )
            , ( "orphans", "no" )
            , ( "sort", "random" )
            , ( "trans_filter", "limit" )
            , ( "trans_orphan", "no" )
            , ( "trans_unapproved", "no" )
            , ( "unapproved", "no" )
            ]


random10 : (Result Http.Error value -> msg) -> (Sentence -> List Sentence -> value) -> Language -> Language -> Cmd msg
random10 toMsg f src dest =
    Http.get
        { url = urlForRandom10 src dest
        , expect = Http.expectJson toMsg <| Json.Decode.field "results" <| Json.Decode.oneOrMore f decodeSentence
        }


type alias Sentence =
    { source : Text
    , translation : String
    , audio : Audio
    }


type alias Text =
    { text : String
    , author : String
    }


type alias Audio =
    { id : Int
    , author : String
    }


decodeSentence : Decoder Sentence
decodeSentence =
    Json.Decode.map3 Sentence
        decodeText
        decodeTranslation
        decodeAudio


decodeText : Decoder Text
decodeText =
    Json.Decode.map2 Text
        (Json.Decode.field "text" Json.Decode.string)
        (Json.Decode.at [ "user", "username" ] Json.Decode.string)


decodeTranslations : Decoder (List String)
decodeTranslations =
    Json.Decode.field "translations"
        <| Json.Decode.index 0
            <| Json.Decode.index 0
                <| Json.Decode.field "text" Json.Decode.string


decodeAudio : Decoder Audio
decodeAudio =
    Json.Decode.field "audios"
        <| Json.Decode.index 0
            Json.Decode.map2 Audio
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "author" Json.Decode.string)


downloadAudio : (Result Audio.LoadError Audio.Source -> msg) -> Audio -> AudioCmd msg
downloadAudio toMsg =
    downloadAudioUrl
        >> Audio.loadAudio toMsg


downloadAudioUrl : Audio -> String
downloadAudioUrl audio =
    Url.Builder.crossOrigin
        "https://tatoeba.org"
        [ "en", "audio", "download", String.fromInt audio.id ]
        []
