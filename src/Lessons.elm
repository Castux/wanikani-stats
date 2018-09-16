module Lessons exposing (view)

import Html exposing (Html)
import Html.Attributes


viewLesson ( date, value ) =
    Html.p []
        [ Html.text <| date ++ ": " ++ String.fromInt value ]


view lessons =
    Html.div
        [ Html.Attributes.class "box" ]
        ([ Html.h2 [] [ Html.text "Lessons history" ] ]
            ++ List.map viewLesson lessons
        )
