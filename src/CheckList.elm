module CheckList exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as DE exposing (Decoder, Error(..),  string)


type alias Model =
    { nicknames : List String
    , errorMessage : Maybe String
    }


url : String
url =
    "https://weerasin.github.io/slate/data.json"
-- url =
--     "http://localhost:8080/data.json"

view : Model -> Html Msg
view model =
    section [ class "section"][
        div [ class "columns"][
          viewHeaderCard "FDX5881"
        , viewHeaderCard "KMEM"
        , viewHeaderCard "KBOS"
        , viewClockCard "1845"
        ]
        , div [][
            button [ onClick SendHttpRequest ]
            [ text "Get data from server" ] 
        , viewNicknamesOrError model   
        ]
    ]
    
viewNicknamesOrError : Model -> Html Msg
viewNicknamesOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewNicknames model.nicknames


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch nicknames at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewNicknames : List String -> Html Msg
viewNicknames nicknames =
    div []
        [ h3 [] [ text "Old School Main Characters" ]
        , ul [] (List.map viewNickname nicknames)
        ]


viewNickname : String -> Html Msg
viewNickname nickname =
    li [] [ text nickname ]

viewClockCard : String -> Html Msg
viewClockCard time =
    div [ class "column"][
        p [ class "title"] [ text <| time ++ " Z"]
    ,   p [ class "title"] [ text "T - xx"]
    ]

viewHeaderCard : String -> Html Msg
viewHeaderCard title =
    div [ class "column"][
        p [ class "title"] [ text title]
    ]
    

type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List String))


getNicknames : Cmd Msg
getNicknames =
    Http.get
        { url = url
        , expect = Http.expectJson DataReceived nicknamesDecoder
        }


nicknamesDecoder : Decoder (List String)
nicknamesDecoder =
    DE.list string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getNicknames )

        DataReceived (Ok nicknames) ->
            ( { model | nicknames = nicknames }, Cmd.none )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nicknames = []
      , errorMessage = Nothing
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
