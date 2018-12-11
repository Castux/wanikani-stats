module Api exposing (Lesson, Review, getLessons, getReviews)

import Http
import ISO8601
import Json.Decode as D
import Time


type alias PagesResponse =
    { nextUrl : Maybe String
    , previousUrl : Maybe String
    , perPage : Int
    }


type alias Review =
    { startSrs : Int
    , endSrs : Int
    , createdAt : String
    }


type alias Lesson =
    { startedAt : Maybe Time.Posix
    , subjectId : Int
    , subjectType : String
    }


type alias ApiResponse a =
    { totalCount : Int
    , pages : PagesResponse
    , data : List a
    }


type ApiUrl
    = Route String
    | Full String



-- 60 days in milliseconds


filterDuration =
    1000 * 3600 * 24 * 60


treatString : String -> Maybe Time.Posix
treatString string =
    case ISO8601.fromString string of
        Ok time ->
            Just (ISO8601.toPosix time)

        _ ->
            Nothing


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
    D.map3 Lesson
        (D.map
            (Maybe.andThen treatString)
            (D.at [ "data", "started_at" ] (D.maybe D.string))
        )
        (D.at [ "data", "subject_id" ] D.int)
        (D.at [ "data", "subject_type" ] D.string)


collectionDecoder dataDecoder =
    D.map3 ApiResponse
        (D.field "total_count" D.int)
        (D.field "pages" pagesDecoder)
        (D.field "data" (D.list dataDecoder))


getCollection key url now decoder messageCons =
    let
        startDate =
            (Time.posixToMillis now - filterDuration)
                |> ISO8601.fromTime
                |> ISO8601.toString

        finalUrl =
            case url of
                Full str ->
                    str

                Route str ->
                    "https://api.wanikani.com/v2/"
                        ++ str
                        ++ "?updated_after="
                        ++ startDate

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

        responseHandler response =
            case response of
                Err err ->
                    messageCons [] Nothing

                Ok payload ->
                    case payload.pages.nextUrl of
                        Just nextUrl ->
                            messageCons
                                payload.data
                                (Just (getCollection key (Full nextUrl) now decoder messageCons))

                        Nothing ->
                            messageCons payload.data Nothing
    in
    Http.send responseHandler request


getReviews : String -> Time.Posix -> (List Review -> Maybe (Cmd msg) -> msg) -> Cmd msg
getReviews key now messageCons =
    getCollection key (Route "reviews") now reviewDecoder messageCons


getLessons : String -> Time.Posix -> (List Lesson -> Maybe (Cmd msg) -> msg) -> Cmd msg
getLessons key now messageCons =
    getCollection key (Route "assignments") now lessonDecoder messageCons
