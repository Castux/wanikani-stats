module Main exposing (main)

import Api
import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Http
import Lessons
import Matrix
import Task
import Time
import Url
import Url.Parser exposing ((<?>))
import Url.Parser.Query


type Message
    = NewKey String
    | GotReviews (List Api.Review)
    | GotLessons (List Api.Lesson)
    | GotSolution (List Float)
    | GotTimezone Time.Zone
    | NewLessonRate String


type alias State =
    { key : String
    , message : Maybe String
    , reviews : List Api.Review
    , lessons : List Api.Lesson
    , zone : Time.Zone

    -- computed
    , rates : Maybe (List Float)

    -- UI
    , lessonRate : Float
    , lessonRateString : String
    }


initState : State
initState =
    { key = ""
    , message = Nothing
    , reviews = []
    , lessons = []
    , zone = Time.utc
    , rates = Nothing
    , lessonRate = 10
    , lessonRateString = "10"
    }


parseUrl : String -> Maybe String
parseUrl stringUrl =
    let
        pathParser =
            Url.Parser.oneOf
                [ Url.Parser.map "" Url.Parser.top
                , Url.Parser.string
                ]

        parser =
            pathParser
                <?> Url.Parser.Query.string "key"
                |> Url.Parser.map (\_ s -> s)

        parse url =
            case Url.Parser.parse parser url of
                Just (Just key) ->
                    Just key

                _ ->
                    Nothing
    in
    stringUrl
        |> Url.fromString
        |> Maybe.andThen parse


startLoading key =
    ( { initState | key = key, message = Just "Loading..." }
    , Cmd.batch
        [ -- Api.getCollection key (Route "reviews") Api.reviewDecoder GotReviewsApiResponse
          -- Api.getCollection key (Route "assignments") Api.lessonDecoder GotLessonsApiResponse
          Task.perform GotTimezone Time.here
        ]
    )


init : String -> ( State, Cmd Message )
init url =
    case parseUrl url of
        Just key ->
            startLoading key

        Nothing ->
            ( initState, Cmd.none )


update msg state =
    ( state, Cmd.none )


subscriptions state =
    Sub.none


view state =
    Html.div [] [ Html.p [] [ Html.text "LoL" ] ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
