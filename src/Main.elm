port module Main exposing (main)


import Html exposing (Html)
import Http
import Regex
import Json.Decode
import Json.Encode
import Time


import Audio exposing (Audio, AudioData, AudioCmd)
import Duration
import Regex.Unicode


import Tatoeba


type Model
    = Idle
    | Error String
    | LoadingSentences
    | Gaming GameData


type alias GameData =
    { state : GameState
    , current : Tatoeba.Sentence
    , sentences : List Tatoeba.Sentence
    }


type GameState
    = LoadingAudio
    | PreppingFirstPlay Audio.Source
    | FirstPlay Audio.Source Time.Posix
    | Entering InputState
    | Transition


type alias InputState =
    { displaySkeleton : String
    , displayedAnswer : String
    , input : String
    , correct : Bool
    , audioSource : Audio.Source
    , audioState : AudioState
    }


type AudioState
    = Playing Time.Posix
    | Silence


type Msg
    = Next
    | GotSentences (Result Http.Error (NonEmptyList Sentence))
    | GotAudio (Result Http.Error Audio.Source)
    | GotTime Time.Posix
    | FinishedPlayback ()
    | UpdateInput String


type NonEmptyList a
    = NonEmptyList a (List a)


init : Flags -> ( Model, Cmd Msg, AudioCmd Msg )
init _ =
    always ( Idle, Cmd.none, Audio.cmdNone )


view : AudioData -> Model -> Html Msg
view _ model = 
    case model of
        Idle ->

        Error e ->

        LoadingSentences ->

        Gaming gameData ->
            case gameData of
                LoadingAudio ->

                PreppingFirstPlay _ ->

                FirstPlay _ _ ->

                Entering inputState ->

                Transition ->


audio : AudioData -> Model -> Audio
audio _ model =
    case model of
        Gaming { state = FirstPlay source time } =
            Audio.audio source time

        Gaming { state = Entering { audioSource, audioState = Playing time } } =
            Audio.audio source time

        _ ->
            Audio.silence


-- languages (temporary for indev)
srcLanguage : Tatoeba.Language
srcLanguage =
    Tatoeba.Esperanto


destLanguage : Tatoeba.Language
destLanugage =
    Tatoeba.English


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update audioData msg model =
    case ( msg, model ) of
        ( GotSentences result, LoadingSentences ) ->
            result
                |> handleResult
                    ( \ (NonEmptyList current rest) ->
                        ( Gaming { state = LoadingAudio, current = current, sentences = rest }
                        , Cmd.none
                        , Tatoeba.downloadAudio current.audio
                        )
                    )

        ( GotAudio result, Gaming ({ state = LoadingAudio } as gameData) ) ->
            result
                |> handleResult
                    ( \ audio ->
                        ( Gaming { gameData | state = PreppingFirstPlay audio }
                        , Task.perform GotTime Time.now
                        , Audio.cmdNone
                        )
                    )

        ( GotTime time, Gaming gameData ) ->
            case gameData.state of
                PreppingFirstPlay audio ->
                    ( Gaming { gameData | state = FirstPlay audio time }
                    , waitToFinish audioData audio
                    , Audio.cmdNone
                    )

                Entering ({ audioSource, audioState = Silence } as inputState) ->
                    ( Gaming { gameData | inputState = { inputState | audioState = Playing time } }
                    , waitToFinish audioData audioSource
                    , Audio.cmdNone
                    )

        ( FinishedPlayback _, Gaming gameData ) ->
            case gameData.state of
                FirstPlay source _ ->
                    let
                        skeleton = Regex.replace Regex.Unicode.letter (always "_") gameData.current.source.text
                    in
                        Entering
                            { displaySkeleton = skeleton
                            , displayedAnswer = skeleton
                            , input = ""
                            , correct = False
                            , audioSource = source
                            , audioState = Silence
                            }

        ( Next, Idle ) ->
            ( LoadingSentences
            , Tatoeba.random10 GotSentences NonEmptyList srcLanguage destLanguage
            , Audio.cmdNone
            )

        ( Next, Error _ ) ->
            ( Idle
            , Cmd.none
            , Audio.cmdNone
            )

        ( Next, Gaming ({ state = Transition } as gameData) ) ->
            case List.uncons gameData.sentences of
                Just ( current, rest ) ->
                    ( Gaming { state = LoadingAudio, current = current, sentences = sentences }
                    , Cmd.none
                    , Tatoeba.downloadAudio current.audio
                    )

                Nothing ->
                    ( Idle
                    , Cmd.none
                    , Audio.cmdNone
                    )

        ( UpdateInput new, Gaming ({ state = Entering inputState } as gameData) ) ->
            let
                correct =
                    String.startsWith inputState.input gameData.current.source.text
            in
                if correct && String.length inputState.input == String.length gameData.current.source.text then
                    ( Gaming { gameData | state = Transition }
                    , Cmd.none
                    , Audio.cmdNone
                    )
                else
                    ( Gaming
                        { gameData | state =
                            { inputState
                            | displayedAnswer =
                                List.map3 ( \ src skel inp -> if src == inp then src else skel )
                                    (String.toList gameData.current.source.text)
                                    (String.toList inputState.displaySkeleton)
                                    (String.toList inputState.input)
                            , input = new
                            , correct = correct
                            }
                        }
                    , Cmd.none
                    , Audio.cmdNone
                    )

        _ ->
            ( model
            , Cmd.none
            , Audio.cmdNone
            )



-- let's hope this works
waitToFinish : AudioData -> Audio.Source -> Cmd Msg
waitToFinish audioData =
    Audio.length audioData
        >> Duration.inMilliseconds
        >> Process.sleep
        >> Task.perform FinishedPlayback


handleResult : (a -> ( Model, Cmd Msg, AudioCmd Msg )) -> Result Http.Error a -> ( Model, Cmd Msg, AudioCmd Msg )
handleResult f result =
    case result of
        Ok x ->
            f x

        Err e ->
            ( Error <| errorToString e, Cmd.none, Audio.cmdNone )


errorToString : Http.Error -> String
errorToString e =
    case e of
        BadUrl url ->
            "Bad URL: " ++ url

        Timeout ->
            "Timed out"

        NetworkError ->
            "Network error"

        BadStatus status ->
            "Bad status: " ++ status

        BadBody body ->
            "Bad body: " ++ body


port audioPortToJS : Json.Encode.Value -> Cmd msg
port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


main : Program Flags (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , update = cringeUpdate
        , view = view
        , subscriptions = subscriptions
        , audio = audio
        , audioPort = {toJS = audioPortToJS, fromJS = audioPortFromJS}
        }
