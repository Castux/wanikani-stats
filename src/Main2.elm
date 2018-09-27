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


flip f x y =
    f y x


levelNamesFull =
    [ "apprentice 1"
    , "apprentice 2"
    , "apprentice 3"
    , "apprentice 4"
    , "guru 1"
    , "guru 2"
    , "master"
    , "enlightened"
    , "burned"
    ]


levelNames =
    [ "A1"
    , "A2"
    , "A3"
    , "A4"
    , "G1"
    , "G2"
    , "M"
    , "E"
    , "burned"
    ]


levelDelays =
    [ 4.0 / 24.0
    , 8.0 / 24.0
    , 1.0
    , 2.0
    , 7.0
    , 14.0
    , 30.0
    , 120.0
    ]


type Message
    = NewKey String
    | GotReviews (List Api.Review) (Maybe (Cmd Message))
    | GotLessons (List Api.Lesson) (Maybe (Cmd Message))
    | GotSolution (List Float)
    | GotTimezone Time.Zone
    | NewLessonRate String


type alias State =
    { key : String
    , reviews : List Api.Review
    , lessons : List Api.Lesson
    , zone : Time.Zone

    -- computed
    , reviewCounts : Counts
    , rates : Maybe (List Float)

    -- UI
    , lessonRate : Float
    , lessonRateString : String
    }


type alias Counts =
    Dict ( Int, Int ) Int


type alias Probabilities =
    Dict ( Int, Int ) Float


initState : State
initState =
    { key = ""
    , reviews = []
    , lessons = []
    , zone = Time.utc
    , reviewCounts = Dict.empty
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
    ( { initState | key = key }
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


accumulateReview review counts =
    Dict.update
        ( review.startSrs, review.endSrs )
        (Maybe.withDefault 0 >> (+) 1 >> Just)
        counts


addReviews state data =
    { state
        | reviews = state.reviews ++ data
        , reviewCounts = List.foldr accumulateReview state.reviewCounts data
    }


computeProbabilities : Counts -> Probabilities
computeProbabilities counts =
    let
        add : ( Int, Int ) -> Int -> Dict Int Int -> Dict Int Int
        add ( start, end ) amount acc =
            Dict.update
                start
                (Maybe.withDefault 0 >> (+) amount >> Just)
                acc

        totals =
            Dict.foldr add Dict.empty counts
    in
    Dict.map
        (\( start, end ) value ->
            case Dict.get start totals of
                Just total ->
                    toFloat value / toFloat total

                Nothing ->
                    1.0
        )
        counts


fixProbabilities : Probabilities -> Probabilities
fixProbabilities probas =
    let
        fix : ( Int, Int ) -> Probabilities -> Probabilities
        fix index dict =
            Dict.update index (Just << Maybe.withDefault 1.0) dict
    in
    List.range 1 8
        |> List.map (\x -> ( x, x + 1 ))
        |> List.foldr fix probas


update msg state =
    case msg of
        NewKey key ->
            startLoading key

        GotTimezone zone ->
            ( { state | zone = zone }, Cmd.none )

        GotLessons data nextCmd ->
            ( { state | lessons = state.lessons ++ data }
            , nextCmd |> Maybe.withDefault Cmd.none
            )

        GotReviews data nextCmd ->
            ( addReviews state data
            , nextCmd |> Maybe.withDefault Cmd.none
            )

        _ ->
            ( state, Cmd.none )


subscriptions state =
    Sub.none



-- Views


viewKeyBox state =
    Html.div
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
        ]


format : Int -> Float -> String
format n f =
    let
        n2 =
            toFloat n
    in
    f * (10.0 ^ n2) |> round |> toFloat |> (\x -> x / 10.0 ^ n2) |> String.fromFloat


toPercentage : Float -> String
toPercentage f =
    f * 100.0 |> format 2 |> flip (++) "%"


viewProbaHeaders =
    levelNames
        |> List.map ((++) "To ")
        |> (::) ""
        |> List.map (Html.text >> List.singleton >> Html.th [])
        |> Html.tr []


viewProbaRow probas row =
    List.range 1 9
        |> List.map (\dest -> Dict.get ( row, dest ) probas)
        |> List.map (Maybe.map toPercentage >> Maybe.withDefault "" >> Html.text >> List.singleton >> Html.td [])
        |> (::) (Html.th [] [ List.drop (row - 1) levelNames |> List.head |> Maybe.withDefault "" |> (++) "From " |> Html.text ])
        |> Html.tr []


viewProbas probas =
    Html.table
        []
        ([ viewProbaHeaders ] ++ (List.range 1 8 |> List.map (viewProbaRow probas)))


viewProbasBox state =
    Html.div
        [ Html.Attributes.class "box" ]
        [ Html.h2 [] [ Html.text "Accuracy" ]
        , viewProbas (fixProbabilities <| computeProbabilities state.reviewCounts)
        , Html.p [] [ Html.text <| "(based on " ++ String.fromInt (List.length state.reviews) ++ " reviews)" ]
        ]


view state =
    Html.div
        [ Html.Attributes.class "main" ]
        [ Html.h1 [ Html.Attributes.class "box" ] [ Html.text "Wanikani accuracy and review pacing" ]
        , viewKeyBox state
        , viewProbasBox state
        ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
