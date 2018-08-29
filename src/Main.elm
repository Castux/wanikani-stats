module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as D


type Message
    = NewKey String
    | GotApiResponse (Result Http.Error (ApiResponse Review))


type alias Counts =
    Dict ( Int, Int ) Int


type alias Probabilities =
    Dict ( Int, Int ) Float


type alias State =
    { key : String
    , errorMsg : Maybe String
    , counts : Counts
    }


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


initState =
    { key = "f0805f70-97af-49aa-a85f-e264e3c489ec"
    , errorMsg = Nothing
    , counts = emptyCounts
    }


init : () -> ( State, Cmd Message )
init flags =
    ( initState, Cmd.none )


update msg state =
    case msg of
        NewKey key ->
            ( { initState | key = key }, getStats key Nothing )

        GotApiResponse (Ok resp) ->
            let
                cmd =
                    case resp.pages.nextUrl of
                        Just nextUrl ->
                            getStats state.key (Just nextUrl)

                        Nothing ->
                            Cmd.none
            in
            ( { state | counts = List.foldr countReview state.counts resp.data }, cmd )

        GotApiResponse (Err resp) ->
            ( { state | errorMsg = Just (Debug.toString resp) }, Cmd.none )


view state =
    Html.div
        []
        [ Html.input
            [ Html.Attributes.placeholder "API v2 key"
            , Html.Attributes.value state.key
            , Html.Events.onInput NewKey
            ]
            []
        , Html.div [] [ state.counts |> Debug.toString |> Html.text ]
        , Html.div [] [ state.counts |> computeProbabilities |> Debug.toString |> Html.text ]
        , Html.div [] [ state.errorMsg |> Maybe.withDefault "" |> Html.text ]
        ]


subscriptions state =
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


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
