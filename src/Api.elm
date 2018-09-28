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
    }


type alias ApiResponse a =
    { totalCount : Int
    , pages : PagesResponse
    , data : List a
    }


type ApiUrl
    = Route String
    | Full String


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
    D.map (Maybe.andThen treatString >> Lesson)
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

        responseHandler response =
            case response of
                Err err ->
                    messageCons [] Nothing

                Ok payload ->
                    case payload.pages.nextUrl of
                        Just nextUrl ->
                            messageCons
                                payload.data
                                (Just (getCollection key (Full nextUrl) decoder messageCons))

                        Nothing ->
                            messageCons payload.data Nothing
    in
    Http.send responseHandler request


getReviews : String -> (List Review -> Maybe (Cmd msg) -> msg) -> Cmd msg
getReviews key messageCons =
    getCollection key (Route "reviews") reviewDecoder messageCons


getLessons : String -> (List Lesson -> Maybe (Cmd msg) -> msg) -> Cmd msg
getLessons key messageCons =
    getCollection key (Route "assignments") lessonDecoder messageCons
