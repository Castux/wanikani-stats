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
    , probas : Probabilities
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
        , viewProbas state.probas
        , Html.p [] [ Html.text <| "(based on " ++ String.fromInt (List.length state.reviews) ++ " reviews)" ]
        ]


viewInput state =
    Html.div
        [ Html.Attributes.class "box" ]
        [ Html.h2 [] [ Html.text "Lessons per day" ]
        , Html.input
            [ Html.Attributes.placeholder "Lessons per day"
            , Html.Attributes.value state.lessonRateString
            , Html.Events.onInput NewLessonRate
            ]
            []
        ]


viewRates rates lessonRate =
    let
        headersRow =
            levelNames
                |> List.take 8
                |> List.map (Html.text >> List.singleton >> Html.th [])
                |> Html.tr []

        ratesRow =
            rates
                |> List.map ((*) lessonRate >> format 2 >> Html.text >> List.singleton >> Html.td [])
                |> Html.tr []
    in
    Html.div
        [ Html.Attributes.class "box" ]
        [ Html.h2 [] [ Html.text "Reviews per day" ]
        , Html.p [] [ Html.text "(to keep up with the lessons)" ]
        , Html.table
            []
            [ headersRow, ratesRow ]
        , Html.p
            []
            [ Html.b [] [ Html.text "Apprentice: " ]
            , rates |> List.take 4 |> List.sum |> (*) lessonRate |> format 2 |> Html.text
            ]
        , Html.p
            []
            [ Html.b [] [ Html.text "Total: " ]
            , rates |> List.sum |> (*) lessonRate |> format 2 |> Html.text
            ]
        ]


viewQueueSizes rates lessonRate =
    let
        headersRow =
            levelNames
                |> List.take 8
                |> List.map (Html.text >> List.singleton >> Html.th [])
                |> Html.tr []

        sizes =
            List.map2 (*) rates levelDelays
                |> List.map ((*) lessonRate)

        queuesRow =
            sizes
                |> List.map (format 2 >> Html.text >> List.singleton >> Html.td [])
                |> Html.tr []
    in
    Html.div [ Html.Attributes.class "box" ]
        [ Html.h2 [] [ Html.text "Average number of non burned items" ]
        , Html.table
            []
            [ headersRow, queuesRow ]
        , Html.p
            []
            [ Html.b [] [ Html.text "Apprentice: " ]
            , sizes |> List.take 4 |> List.sum |> format 2 |> Html.text
            ]
        , Html.p
            []
            [ Html.b [] [ Html.text "Total: " ]
            , sizes |> List.sum |> format 2 |> Html.text
            ]
        ]


viewBurnTime rates =
    let
        totalSize =
            List.map2 (*) rates levelDelays |> List.sum
    in
    Html.div
        [ Html.Attributes.class "box" ]
        [ Html.h2 [] [ Html.text "Time to burn" ]
        , Html.p
            []
            [ Html.b [] [ Html.text "Average burn time: " ]
            , totalSize |> format 2 |> flip (++) " days" |> Html.text
            ]
        ]


viewResults state =
    case state.rates of
        Just rates ->
            [ viewRates rates state.lessonRate
            , viewQueueSizes rates state.lessonRate
            , viewBurnTime rates

            --, Lessons.view state.lessonDates
            ]

        Nothing ->
            []


view state =
    let
        top =
            [ Html.h1 [ Html.Attributes.class "box" ] [ Html.text "Wanikani accuracy and review pacing" ]
            , viewKeyBox state
            , viewProbasBox state
            , viewInput state
            ]

        results =
            viewResults state

        elements =
            top ++ results
    in
    Html.div
        [ Html.Attributes.class "main" ]
        elements


subscriptions state =
    Matrix.solution GotSolution


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
