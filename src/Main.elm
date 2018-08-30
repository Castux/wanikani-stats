module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as D
import Matrix


flip f x y =
    f y x


type Message
    = NewKey String
    | GotApiResponse (Result Http.Error (ApiResponse Review))
    | GotSolution (List Float)
    | NewLessonRate String


type alias Counts =
    Dict ( Int, Int ) Int


type alias Probabilities =
    Dict ( Int, Int ) Float


type alias LoadingState =
    { key : String
    , errorMsg : Maybe String
    , counts : Counts
    }


type alias LoadedState =
    { probas : Probabilities
    , rates : Maybe (List Float)
    , lessonRate : Float
    , lessonRateString : String
    }


type State
    = Loading LoadingState
    | Loaded LoadedState


type alias PagesResponse =
    { nextUrl : Maybe String
    , previousUrl : Maybe String
    , perPage : Int
    }


type alias ResourceResponse a =
    { data : a }


type alias Review =
    { startSrs : Int
    , endSrs : Int
    }


type alias ApiResponse a =
    { totalCount : Int
    , pages : PagesResponse
    , data : List a
    }


levelNames =
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


initState =
    { key = "f0805f70-97af-49aa-a85f-e264e3c489ec"
    , errorMsg = Nothing
    , counts = emptyCounts
    }


init : () -> ( State, Cmd Message )
init flags =
    ( Loading initState, getStats initState.key Nothing )


update : Message -> State -> ( State, Cmd Message )
update msg state =
    case ( msg, state ) of
        ( NewKey key, _ ) ->
            ( Loading { initState | key = key }, getStats key Nothing )

        ( GotApiResponse (Ok resp), Loading loadingState ) ->
            let
                newCounts =
                    List.foldr countReview loadingState.counts resp.data
            in
            case resp.pages.nextUrl of
                Just nextUrl ->
                    ( Loading { loadingState | counts = newCounts, errorMsg = Nothing }, getStats loadingState.key (Just nextUrl) )

                Nothing ->
                    let
                        probas =
                            fixProbabilities <| computeProbabilities newCounts
                    in
                    ( Loaded (LoadedState probas Nothing 1.0 "1"), Matrix.solve { a = Matrix.makepProblemMatrix 8 probas, b = [ -1.0, 0, 0, 0, 0, 0, 0, 0 ] } )

        ( GotApiResponse (Err resp), Loading loadingState ) ->
            ( Loading { loadingState | errorMsg = Just "Error!" }, Cmd.none )

        ( GotSolution rates, Loaded loadedState ) ->
            ( Loaded { loadedState | rates = Just rates }, Cmd.none )

        ( NewLessonRate rateStr, Loaded loadedState ) ->
            let
                newRate =
                    rateStr |> String.toFloat |> Maybe.withDefault loadedState.lessonRate
            in
            ( Loaded { loadedState | lessonRate = newRate, lessonRateString = rateStr }, Cmd.none )

        _ ->
            ( state, Cmd.none )


viewLoading state =
    Html.div
        []
        [ Html.input
            [ Html.Attributes.placeholder "API v2 key"
            , Html.Attributes.value state.key
            , Html.Events.onInput NewKey
            ]
            []
        , Html.div [] [ state.counts |> Dict.values |> List.sum |> String.fromInt |> Html.text ]
        , Html.div [] [ state.errorMsg |> Maybe.withDefault "" |> Html.text ]
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
    Html.table
        []
        [ headersRow, ratesRow ]


viewRatesTotals rates lessonRate =
    Html.table
        []
        [ Html.tr []
            [ Html.th [] [ Html.text "Apprentice reviews/day" ]
            , Html.th [] [ Html.text "Total reviews/day" ]
            ]
        , Html.tr []
            [ Html.td [] [ rates |> List.take 4 |> List.sum |> (*) lessonRate |> format 2 |> Html.text ]
            , Html.td [] [ rates |> List.sum |> (*) lessonRate |> format 2 |> Html.text ]
            ]
        ]


viewQueueSizes rates lessonRate =
    let
        headersRow =
            levelNames
                |> List.take 8
                |> List.map (Html.text >> List.singleton >> Html.th [])
                |> Html.tr []

        queuesRow =
            rates
                |> List.map2 (*) levelDelays
                |> List.map ((*) lessonRate >> format 2 >> Html.text >> List.singleton >> Html.td [])
                |> Html.tr []
    in
    Html.table
        []
        [ headersRow, queuesRow ]


viewQueueSizesTotals rates lessonRate =
    let
        sizes =
            List.map2 (*) rates levelDelays
                |> List.map ((*) lessonRate)
    in
    Html.table
        []
        [ Html.tr []
            [ Html.th [] [ Html.text "Average apprentice items" ]
            , Html.th [] [ Html.text "Average items" ]
            ]
        , Html.tr []
            [ Html.td [] [ sizes |> List.take 4 |> List.sum |> format 2 |> Html.text ]
            , Html.td [] [ sizes |> List.sum |> format 2 |> Html.text ]
            ]
        ]


viewBurnTime rates =
    let
        totalSize =
            List.map2 (*) rates levelDelays |> List.sum
    in
    Html.table
        []
        [ Html.tr []
            [ Html.th [] [ Html.text "Average burn time" ]
            ]
        , Html.tr []
            [ Html.td [] [ totalSize |> format 2 |> flip (++) " days" |> Html.text ]
            ]
        ]


viewLoaded : LoadedState -> Html.Html Message
viewLoaded state =
    let
        alwaysThere =
            [ Html.h1 [] [ Html.text "Accuracy" ]
            , viewProbas state.probas
            , Html.h1 [] [ Html.text "Lessons per day" ]
            , Html.input
                [ Html.Attributes.placeholder "Lessons per day"
                , Html.Attributes.value state.lessonRateString
                , Html.Events.onInput NewLessonRate
                ]
                []
            ]

        ifComputed =
            case state.rates of
                Just rates ->
                    [ Html.h1 [] [ Html.text "Reviews per day" ]
                    , Html.p [] [ Html.text "(to keep up with the lessons)" ]
                    , viewRates rates state.lessonRate
                    , viewRatesTotals rates state.lessonRate
                    , Html.h1 [] [ Html.text "Average number of non burned items" ]
                    , viewQueueSizes rates state.lessonRate
                    , viewQueueSizesTotals rates state.lessonRate
                    , Html.h1 [] [ Html.text "Time to burn" ]
                    , viewBurnTime rates
                    ]

                Nothing ->
                    []
    in
    Html.div
        []
        (alwaysThere ++ ifComputed)


viewProbaHeaders =
    levelNames
        |> List.map ((++) "To ")
        |> (::) ""
        |> List.map (Html.text >> List.singleton >> Html.th [])
        |> Html.tr []


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


view : State -> Html.Html Message
view state =
    case state of
        Loading st ->
            viewLoading st

        Loaded st ->
            viewLoaded st


subscriptions state =
    case state of
        Loaded st ->
            Matrix.solution GotSolution

        _ ->
            Sub.none


pagesDecoder =
    D.map3 PagesResponse
        (D.field "next_url" (D.nullable D.string))
        (D.field "previous_url" (D.nullable D.string))
        (D.field "per_page" D.int)


reviewDecoder =
    D.map2 Review
        (D.at [ "data", "starting_srs_stage" ] D.int)
        (D.at [ "data", "ending_srs_stage" ] D.int)


jsonDecoder =
    D.map3 ApiResponse
        (D.field "total_count" D.int)
        (D.field "pages" pagesDecoder)
        (D.field "data" (D.list reviewDecoder))


getStats key url =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Authorization" ("Bearer " ++ key) ]
                , url = url |> Maybe.withDefault "https://api.wanikani.com/v2/reviews"
                , body = Http.emptyBody
                , expect = Http.expectJson jsonDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send GotApiResponse request


emptyCounts =
    Dict.empty


countReview review counts =
    Dict.update
        ( review.startSrs, review.endSrs )
        (Maybe.withDefault 0 >> (+) 1 >> Just)
        counts


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


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
