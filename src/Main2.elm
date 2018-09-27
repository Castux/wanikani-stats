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
    | GotReviews (List Api.Review) (Cmd Message)
    | GotLessons (List Api.Lesson) (Cmd Message)
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
        [ Api.getReviews key GotReviews
        , Api.getLessons key GotLessons
        , Task.perform GotTimezone Time.here
        ]
    )


init : String -> ( State, Cmd Message )
init url =
    case parseUrl url of
        Just key ->
            startLoading key

        Nothing ->
            ( initState, Cmd.none )


viewKeyBox state =
    Html.div
        [ Html.Attributes.class "main" ]
        [ Html.h1 [ Html.Attributes.class "box" ] [ Html.text "Wanikani accuracy and review pacing" ]
        , Html.div
            [ Html.Attributes.class "box" ]
            [ Html.p []
                [ Html.text "Please enter your API version 2 key. You can find it on "
                , Html.a
                    [ Html.Attributes.href "https://www.wanikani.com/settings/account"
                    , Html.Attributes.target "_blank"
                    ]
                    [ Html.text "your profile page" ]
                , Html.text "."
                ]
            , Html.input
                [ Html.Attributes.placeholder "API v2 key"
                , Html.Attributes.value state.key
                , Html.Events.onInput NewKey
                ]
                []
            , Html.div [] [ state.message |> Maybe.withDefault "" |> Html.text ]
            ]
        ]


update msg state =
    case msg of
        NewKey key ->
            startLoading key

        GotTimezone zone ->
            ( { state | zone = zone }, Cmd.none )

        GotLessons data nextCmd ->
            ( { state | lessons = state.lessons ++ data }, nextCmd )

        GotReviews data nextCmd ->
            ( { state | reviews = state.reviews ++ data }, nextCmd )

        _ ->
            ( state, Cmd.none )


subscriptions state =
    Sub.none


view state =
    Html.div []
        [ viewKeyBox state ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
