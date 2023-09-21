module CheckList exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    Int


initialModel : Model
initialModel =
    0


view : Model -> Html Msg
view model =
    section [ class "section"][
        div [ class "columns"][
          viewHeaderCard "FDX5881"
        , viewHeaderCard "KMEM"
        , viewHeaderCard "KBOS"
        , viewClockCard "1845"
        ]
    ]


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
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }