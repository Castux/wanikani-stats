module Api exposing (ApiResponse, ApiUrl(..), Lesson, PagesResponse, ResourceResponse, Review)

import Http
import Json.Decode as D


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
