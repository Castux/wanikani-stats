module State exposing (Counts, Message(..), Probabilities, State)

import Api
import Dict exposing (Dict)
import Time


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
