module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as D
import Matrix
import Url
import Url.Parser exposing ((<?>))
import Url.Parser.Query


flip f x y =
    f y x


type Message
    = NewKey String
    | GotReviewsApiResponse (Result Http.Error (ApiResponse Review))
    | GotLessonsApiResponse (Result Http.Error (ApiResponse Lesson))
    | GotSolution (List Float)
    | NewLessonRate String


type alias Counts =
    Dict ( Int, Int ) Int


type alias Probabilities =
    Dict ( Int, Int ) Float


type alias LoadingState =
    { key : String
    , message : Maybe String
    , counts : Counts
    , lessons : Dict String Int
    , callsFinished : Int
    }


type alias LoadedState =
    { probas : Probabilities
    , reviewCount : Int
    , rates : Maybe (List Float)
    , lessonRate : Float
    , lessonRateString : String
    , lessonDates : List ( String, Int )
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
    , createdAt : String
    }


type alias Lesson =
    { startedAt : Maybe String
    }


type alias ApiResponse a =
    { totalCount : Int
    , pages : PagesResponse
    , data : List a
    }


type ApiUrl
    = Route String
    | Full String


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


initState =
    { key = ""
    , message = Nothing
    , counts = emptyCounts
    , lessons = Dict.empty
    , callsFinished = 0
    }


init : String -> ( State, Cmd Message )
init url =
    case parseUrl url of
        Just key ->
            startLoading key

        Nothing ->
            ( Loading initState, Cmd.none )


startLoading key =
    ( Loading { initState | key = key, message = Just "Loading..." }
    , Cmd.batch
        [ getCollection key (Route "reviews") reviewDecoder GotReviewsApiResponse
        , getCollection key (Route "assignments") lessonDecoder GotLessonsApiResponse
        ]
    )


checkState loadingState =
    if loadingState.callsFinished == 2 then
        let
            probas =
                fixProbabilities <| computeProbabilities loadingState.counts

            total =
                loadingState.counts |> Dict.values |> List.sum

            lessons =
                Dict.toList loadingState.lessons
        in
        ( Loaded (LoadedState probas total Nothing 1.0 "1" lessons)
        , Matrix.solve { a = Matrix.makepProblemMatrix 8 probas, b = [ -1.0, 0, 0, 0, 0, 0, 0, 0 ] }
        )

    else
        ( Loading loadingState, Cmd.none )


update : Message -> State -> ( State, Cmd Message )
update msg state =
    case ( msg, state ) of
        ( NewKey key, _ ) ->
            startLoading key

        ( GotReviewsApiResponse (Ok resp), Loading loadingState ) ->
            let
                newCounts =
                    List.foldr countReview loadingState.counts resp.data

                newState =
                    { loadingState | counts = newCounts }
            in
            case resp.pages.nextUrl of
                Just nextUrl ->
                    ( Loading newState, getCollection loadingState.key (Full nextUrl) reviewDecoder GotReviewsApiResponse )

                Nothing ->
                    checkState { newState | callsFinished = newState.callsFinished + 1 }

        ( GotReviewsApiResponse (Err resp), Loading loadingState ) ->
            ( Loading { loadingState | message = Just "Error!" }, Cmd.none )

        ( GotLessonsApiResponse (Ok resp), Loading loadingState ) ->
            let
                newLessons =
                    resp.data
                        |> List.filterMap .startedAt
                        |> List.foldl countLesson loadingState.lessons

                newState =
                    { loadingState | lessons = newLessons }
            in
            case resp.pages.nextUrl of
                Just nextUrl ->
                    ( Loading newState, getCollection loadingState.key (Full nextUrl) lessonDecoder GotLessonsApiResponse )

                Nothing ->
                    checkState { newState | callsFinished = newState.callsFinished + 1 }

        ( GotLessonsApiResponse (Err resp), Loading loadingState ) ->
            ( Loading { loadingState | message = Just "Error!" }, Cmd.none )

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


viewLesson ( date, value ) =
    Html.p []
        [ Html.text <| date ++ ": " ++ String.fromInt value ]


viewLessons lessons =
    Html.div
        [ Html.Attributes.class "box" ]
        ([ Html.h2 [] [ Html.text "Lessons history" ] ]
            ++ List.map viewLesson lessons
        )


viewLoaded : LoadedState -> Html.Html Message
viewLoaded state =
    let
        alwaysThere =
            [ Html.h1 [ Html.Attributes.class "box" ] [ Html.text "Wanikani accuracy and review pacing" ]
            , Html.div
                [ Html.Attributes.class "box" ]
                [ Html.h2 [] [ Html.text "Accuracy" ]
                , viewProbas state.probas
                , Html.p [] [ Html.text <| "(based on " ++ String.fromInt state.reviewCount ++ " reviews)" ]
                ]
            , Html.div
                [ Html.Attributes.class "box" ]
                [ Html.h2 [] [ Html.text "Lessons per day" ]
                , Html.input
                    [ Html.Attributes.placeholder "Lessons per day"
                    , Html.Attributes.value state.lessonRateString
                    , Html.Events.onInput NewLessonRate
                    ]
                    []
                ]
            ]

        ifComputed =
            case state.rates of
                Just rates ->
                    [ viewRates rates state.lessonRate
                    , viewQueueSizes rates state.lessonRate
                    , viewBurnTime rates
                    , viewLessons state.lessonDates
                    ]

                Nothing ->
                    []
    in
    Html.div
        [ Html.Attributes.class "main" ]
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
    D.map3 Review
        (D.at [ "data", "starting_srs_stage" ] D.int)
        (D.at [ "data", "ending_srs_stage" ] D.int)
        (D.at [ "data", "created_at" ] D.string)


lessonDecoder =
    D.map Lesson
        (D.at [ "data", "started_at" ] (D.maybe D.string))


collectionDecoder dataDecoder =
    D.map3 ApiResponse
        (D.field "total_count" D.int)
        (D.field "pages" pagesDecoder)
        (D.field "data" (D.list dataDecoder))


getCollection key url decoder messageCons =
    let
        finalUrl =
            case url of
                Full str ->
                    str

                Route str ->
                    "https://api.wanikani.com/v2/" ++ str

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Authorization" ("Bearer " ++ key) ]
                , url = finalUrl
                , body = Http.emptyBody
                , expect = Http.expectJson (collectionDecoder decoder)
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send messageCons request


emptyCounts =
    Dict.empty


countReview review counts =
    Dict.update
        ( review.startSrs, review.endSrs )
        (Maybe.withDefault 0 >> (+) 1 >> Just)
        counts


countLesson lessonDate counts =
    Dict.update
        (String.left 10 lessonDate)
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
