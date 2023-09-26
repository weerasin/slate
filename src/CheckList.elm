module CheckList exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
--import Json.Decode exposing (bool)


type alias Model =
    { 
        flightInfo : FlightInfo
        , isEditorVisible : Bool
        , time : Time.Posix
    }

type alias FlightInfo =
    { 
        flightNumber : String
        , departureAirport : String
        , arrivalAirport : String
        , departureTime : (Int, Int)
        , depatureTotalMinutes : Int
        , departureIsNextUTCDay : Bool
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
           , input [ 
            class "input"
            , type_ "number"
            , maxlength 4
            , placeholder "Departure Time"
            , onInput UpdateDepartureTime ] []
            , label [ class "checkbox"] [
                text ""
                ,input [ class "input", type_ "checkbox", onCheck UpdateDepartureIsNextUTCDay][]
            ]
            , input [ class "input", type_ "text", placeholder "Gate", onInput UpdateGate ] []
       ]
    ]


viewCardSection : Model -> Html Msg
viewCardSection model =
    section [ class "section"][
        div [ class "columns"][
          viewHeaderCard ("FDX" ++ model.flightInfo.flightNumber) <| model.flightInfo.gate
        , viewLinkCard <| model.flightInfo.departureAirport
        , viewLinkCard <| model.flightInfo.arrivalAirport
        , viewClockCard  model
        ] 
    ]
    

viewClockCard : Model -> Html Msg
viewClockCard model =
    let
        --time = model.departureTime |> getTimeString
        timeToDeparture = getTimeToDeparture model.time model.flightInfo.depatureTotalMinutes model.flightInfo.departureIsNextUTCDay
        timeString = if model.flightInfo.departureIsNextUTCDay then
            (model.flightInfo.departureTime |> getTimeString) ++ " Z +1"
            else
                (model.flightInfo.departureTime |> getTimeString) ++ " Z"
    in
    div [ class "column"][
        if timeToDeparture <= 0 then
            p [ class "title", style "color" "green"] [ text ("T " ++ String.fromInt timeToDeparture )]
        else
            p [ class "title", style "color" "red"] [ text ("T+" ++ String.fromInt timeToDeparture )]
        ,p [ class "subtitle"] [ text timeString]   
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
   | UpdateDepartureIsNextUTCDay Bool 
   | Tick Time.Posix





update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowEditor ->
            ({ model| isEditorVisible = True } , Cmd.none )
        HideEditor ->
            ({ model| isEditorVisible = False } , Cmd.none )
        UpdateFlightNumber flightNumber ->
            let 
                fi = model.flightInfo  
                newfi = { fi | flightNumber = flightNumber }           
            in
            ({ model| flightInfo = newfi } , Cmd.none )
        UpdateDepartureAirport departureAirport ->
            let 
                fi = model.flightInfo  
                newfi = { fi | departureAirport = String.toUpper departureAirport }
            in
            ({ model| flightInfo = newfi} , Cmd.none )
        UpdateArrivalAirport arrivalAirport ->
            let 
                fi = model.flightInfo  
                newfi = { fi | arrivalAirport = String.toUpper arrivalAirport }
            in
            ({ model| flightInfo = newfi} , Cmd.none )
        UpdateDepartureTime departureTime ->
            let
                fi = model.flightInfo  
                newfi = { fi | departureTime = (departureTime |> getValidTime )
                    , depatureTotalMinutes = (departureTime |> getValidTime |> getTotalMinutes)
                        }
            in
            ({ model| flightInfo = newfi } , Cmd.none )
        UpdateGate gate ->
            let 
                fi = model.flightInfo  
                newfi = { fi | gate = gate }
            in
            ({ model| flightInfo = newfi } , Cmd.none )
        UpdateDepartureIsNextUTCDay isNextDay ->
            let 
                fi = model.flightInfo
                newfi = { fi | departureIsNextUTCDay = isNextDay }  
            in
            ({ model| flightInfo = newfi } , Cmd.none )
        Tick time ->
            ({ model| time = time } , Cmd.none )



init : () -> ( Model, Cmd Msg )
init _ =
    ( 
      {
        flightInfo = {  flightNumber = "0000"
            , departureAirport = "KMEM"
            , arrivalAirport = "KMEM"
            , departureTime = (0,0)
            , depatureTotalMinutes = 0
            , departureIsNextUTCDay = False
            , gate = "UNKN"
          }
        , isEditorVisible = False
        , time = Time.millisToPosix 0
      }  
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every (30*1000) Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Utility Functions
validateTimeLength : String -> Maybe String
validateTimeLength value =
    validateLength 4 value


validateLength : Int -> String -> Maybe String
validateLength length value =
    if String.length value == length then
        Just value
    else
        Nothing


validateHour : Int-> Maybe Int
validateHour value =
    validateMaxVal 23 value

validateMinute : Int -> Maybe Int
validateMinute value =
    validateMaxVal 59 value


validateMaxVal : Int -> Int -> Maybe Int
validateMaxVal maxVal value =
    if value <= maxVal then
        Just value
    else
        Nothing


getHour : String -> Maybe Int
getHour value =
    value
        |> String.left 2
        |> String.toInt
        |> Maybe.andThen validateHour
    

getMinute : String -> Maybe Int 
getMinute value =
    value
        |> String.right 2
        |> String.toInt
        |> Maybe.andThen validateMinute


getValidTime : String -> (Int, Int)
getValidTime value =
    case validateTimeLength value of
        Just validTime ->
            case (getHour validTime, getMinute validTime) of
                (Just h, Just m) ->
                    (h, m)
                _ ->
                    (0, 0)
        Nothing ->
            (0, 0)
   

getTotalMinutes : (Int,Int) -> Int
getTotalMinutes (hour, minute) =
    hour * 60 + minute


getTimeString : (Int, Int) -> String    
getTimeString (hour, minute) =
    String.padLeft 2 '0' (String.fromInt hour) ++ String.padLeft 2 '0' (String.fromInt minute)


getTimeToDeparture : Time.Posix -> Int -> Bool -> Int
getTimeToDeparture currentTime departureTime isNextDay = 
    let
        h  = Time.toHour   Time.utc currentTime
        m =  Time.toMinute Time.utc currentTime
        currentTotalMinutes = getTotalMinutes (h, m)
    in
        if isNextDay then
            currentTotalMinutes - (1440 + departureTime)  
        else
            currentTotalMinutes - departureTime
