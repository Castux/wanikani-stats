module Main exposing (main)

import Api
import Browser
import Dict exposing (Dict)
import Http
import Lessons
import Matrix
import State exposing (..)
import Task
import Time
import Url
import Url.Parser exposing ((<?>))
import Url.Parser.Query
import View


initState : State
initState =
    { key = ""
    , reviews = []
    , lessons = []
    , zone = Time.utc
    , reviewCounts = Dict.empty
    , probas = Dict.empty
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
        [ Task.perform GotTimezone Time.here
        , Task.perform GotTime Time.now
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
    let
        newCounts =
            List.foldr accumulateReview state.reviewCounts data
    in
    { state
        | reviews = state.reviews ++ data
        , reviewCounts = newCounts
        , probas = newCounts |> computeProbabilities |> fixProbabilities
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


computeRates probas =
    Matrix.solve { a = Matrix.makepProblemMatrix 8 probas, b = [ -1.0, 0, 0, 0, 0, 0, 0, 0 ] }


update msg state =
    case msg of
        NewKey key ->
            startLoading key

        GotTimezone zone ->
            ( { state | zone = zone }, Cmd.none )

        GotTime time ->
            ( state
            , Cmd.batch
                [ Api.getReviews state.key time GotReviews

                --, Api.getLessons state.key time GotLessons
                ]
            )

        NewLessonRate rateStr ->
            let
                newRate =
                    rateStr |> String.toFloat |> Maybe.withDefault state.lessonRate
            in
            ( { state | lessonRate = newRate, lessonRateString = rateStr }, Cmd.none )

        GotLessons data nextCmd ->
            ( { state | lessons = state.lessons ++ data }
            , nextCmd |> Maybe.withDefault Cmd.none
            )

        GotReviews data nextCmd ->
            let
                newState =
                    addReviews state data
            in
            ( newState
            , nextCmd |> Maybe.withDefault (computeRates newState.probas)
            )

        GotSolution rates ->
            ( { state | rates = Just rates }, Cmd.none )


subscriptions state =
    Matrix.solution GotSolution


main =
    Browser.element
        { init = init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        }
