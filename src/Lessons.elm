module Lessons exposing (view)

import Api
import Dict
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Time exposing (Month(..))


toNumericalMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


toDateTriplet zone posix =
    ( Time.toYear zone posix, toNumericalMonth <| Time.toMonth zone posix, Time.toDay zone posix )


millisPerDay =
    1000 * 3600 * 24


groupByDay zone lessons =
    let
        decorated =
            lessons
                |> List.filterMap
                    (\l ->
                        case l.startedAt of
                            Just date ->
                                Just ( toDateTriplet zone date, l )

                            Nothing ->
                                Nothing
                    )

        insert ( triplet, lesson ) dict =
            Dict.update
                triplet
                (Maybe.withDefault [] >> (::) lesson >> Just)
                dict

        grouped =
            List.foldl insert Dict.empty decorated

        firstDay =
            grouped
                |> Dict.toList
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.andThen List.head
                |> Maybe.andThen .startedAt
                |> Maybe.map Time.posixToMillis
                |> Maybe.withDefault 0

        lastDay =
            grouped
                |> Dict.toList
                |> List.Extra.last
                |> Maybe.map Tuple.second
                |> Maybe.andThen List.head
                |> Maybe.andThen .startedAt
                |> Maybe.map Time.posixToMillis
                |> Maybe.withDefault 0

        insertEmpty triplet dict =
            Dict.update triplet (Maybe.withDefault [] >> Just) dict

        fixed =
            List.range 0 ((lastDay - firstDay) // millisPerDay + 1)
                |> List.map ((*) millisPerDay)
                |> List.map ((+) firstDay)
                |> List.map Time.millisToPosix
                |> List.map (toDateTriplet zone)
                |> List.foldl insertEmpty grouped
    in
    fixed


view : Time.Zone -> List Api.Lesson -> Html msg
view zone lessons =
    let
        days =
            groupByDay zone lessons
                |> Dict.toList
                |> List.map (\( k, v ) -> ( k, List.length v ))
    in
    Html.div
        [ Html.Attributes.class "box" ]
        [ Html.h2 [] [ Html.text "Lessons history" ]
        , Html.p [] [ Html.text <| Debug.toString days ]
        ]
