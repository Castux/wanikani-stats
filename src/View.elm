module View exposing (view)

import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Lessons
import State exposing (..)


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
        , Html.p [] [ Html.text <| "(based on " ++ String.fromInt (List.length state.reviews) ++ " reviews in the past 60 days)" ]
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

        lessons =
            [ Lessons.view state.zone state.lessons ]

        elements =
            top ++ results ++ lessons
    in
    Html.div
        [ Html.Attributes.class "main" ]
        elements
