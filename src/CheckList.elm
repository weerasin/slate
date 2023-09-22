module CheckList exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { 
        flightNumber : String
        , departureAirport : String
        , arrivalAirport : String
        , isEditorVisible : Bool
        , departureTime : String
        , gate : String
    }


view : Model -> Html Msg
view model =
    div[][ 
        viewHeaderEdit model
        , viewCardSection model
    ]


viewHeaderEdit :Model -> Html Msg
viewHeaderEdit model = 
    nav[ class "panel" 
        , classList [ ("is-hidden", model.isEditorVisible == False)]
    ] [ 
       div [ class "panel-heading" , onClick HideEditor] [ 
            p [ class "panel-title"] [ text "Edit Heading" ]
       ]
       , div [ class "panel-block"] [
           input [ class "input", type_ "text", placeholder "Flight Number", onInput UpdateFlightNumber] []
           , input [ class "input", type_ "text", placeholder "Departure Airport", onInput UpdateDepartureAirport] []
           , input [ class "input", type_ "text", placeholder "Arrival Airport", onInput UpdateArrivalAirport ] []
           , input [ class "input", type_ "text", placeholder "Departure Time", onInput UpdateDepartureTime ] []
           , input [ class "input", type_ "text", placeholder "Gate", onInput UpdateGate ] []
       ]
    ]


viewCardSection : Model -> Html Msg
viewCardSection model =
    section [ class "section"][
        div [ class "columns"][
          viewHeaderCard ("FDX" ++ model.flightNumber) <| model.gate
        , viewLinkCard <| model.departureAirport
        , viewLinkCard <| model.arrivalAirport
        , viewClockCard  <| model.departureTime
        ] 
    ]
    

viewClockCard : String -> Html Msg
viewClockCard time =
    div [ class "column"][
        p [ class "title"] [ text <| time ++ " Z"]
    ,   p [ class "title"] [ text "T - xx"]
    ]

viewHeaderCard : String -> String -> Html Msg
viewHeaderCard title gate =
    div [ class "column"][
        p [ class "title", onClick ShowEditor] [ text title]
        , p [ class "subtitle", onClick ShowEditor] [ text ("Gate: " ++ gate)]
    ]

viewLinkCard : String -> Html Msg
viewLinkCard title =
    let
        linkUrl = "https://www.aviationweather.gov/taf/board?ids=" ++ title ++ "&date=&submit=Goto+TAF+board"
    in
    div [ class "column"][
        a [ class "title", href linkUrl, target "_blank" ] [ text title]
    ]

type Msg
   = ShowEditor
   | HideEditor
   | UpdateFlightNumber String
   | UpdateDepartureAirport String
   | UpdateArrivalAirport String
   | UpdateDepartureTime String
   | UpdateGate String





update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowEditor ->
            ({ model| isEditorVisible = True } , Cmd.none )
        HideEditor ->
            ({ model| isEditorVisible = False } , Cmd.none )
        UpdateFlightNumber flightNumber ->
            ({ model| flightNumber = flightNumber } , Cmd.none )
        UpdateDepartureAirport departureAirport ->
            ({ model| departureAirport = String.toUpper departureAirport } , Cmd.none )
        UpdateArrivalAirport arrivalAirport ->
            ({ model| arrivalAirport = String.toUpper arrivalAirport } , Cmd.none )
        UpdateDepartureTime departureTime ->
            ({ model| departureTime = departureTime } , Cmd.none )
        UpdateGate gate ->
            ({ model| gate = gate } , Cmd.none )



init : () -> ( Model, Cmd Msg )
init _ =
    ( 
      {
        flightNumber = "0000"
        , departureAirport = "KMEM"
        , arrivalAirport = "KBOS"
        , isEditorVisible = False
        , departureTime = "1730"
        , gate = "UNKN"
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
