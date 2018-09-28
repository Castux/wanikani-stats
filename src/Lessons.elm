module Lessons exposing (view)

import Api
import Html exposing (Html)
import Html.Attributes
import Time exposing (Month(..))


toNumericalMonth month =
    case month of
        Jan ->
            "1"

        Feb ->
            "2"

        Mar ->
            "3"

        Apr ->
            "4"

        May ->
            "5"

        Jun ->
            "6"

        Jul ->
            "7"

        Aug ->
            "8"

        Sep ->
            "9"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


formatPosix zone posix =
    String.fromInt (Time.toYear zone posix)
        ++ "-"
        ++ toNumericalMonth (Time.toMonth zone posix)
        ++ "-"
        ++ String.fromInt (Time.toDay zone posix)


viewLesson : Time.Zone -> Api.Lesson -> Html msg
viewLesson zone lesson =
    Html.p []
        [ Html.text <|
            case lesson.startedAt of
                Just date ->
                    formatPosix zone date

                Nothing ->
                    "Invalid date"
        ]


view : Time.Zone -> List Api.Lesson -> Html msg
view zone lessons =
    Html.div
        [ Html.Attributes.class "box" ]
        ([ Html.h2 [] [ Html.text "Lessons history" ] ]
            ++ List.map (viewLesson zone) lessons
        )
