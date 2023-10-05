port module CheckList exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Time



--import Json.Decode exposing (bool)


type alias Model =
    { flightInfo : FlightInfo
    , checkListItems : Dict String Bool
    , isEditorVisible : Bool
    , time : Time.Posix
    }


type alias FlightInfo =
    { flightNumber : String
    , departureAirport : String
    , arrivalAirport : String
    , departureTime : String
    , depatureTotalMinutes : Int
    , departureIsNextUTCDay : Bool
    , gate : String
    }


type alias LocalStorage =
    { currentTime : Int
    , flightInfo : FlightInfo
    , checkListItems : Dict String Bool
    }


view : Model -> Html Msg
view model =
    div []
        [ viewHeaderEdit model
        , viewCardSection model
        , viewCheckList model
        ]


viewHeaderEdit : Model -> Html Msg
viewHeaderEdit model =
    nav
        [ class "panel"
        , classList [ ( "is-hidden", model.isEditorVisible == False ) ]
        ]
        [ div [ class "panel-heading", onClick HideEditor ]
            [ p [ class "panel-title" ] [ text "Edit Heading" ]
            ]
        , div [ class "panel-block" ]
            [ input [ class "input", type_ "text", placeholder "Flight Number", onInput UpdateFlightNumber ] []
            , input [ class "input", type_ "text", placeholder "Departure Airport", onInput UpdateDepartureAirport ] []
            , input [ class "input", type_ "text", placeholder "Arrival Airport", onInput UpdateArrivalAirport ] []
            , input
                [ class "input"
                , type_ "number"
                , maxlength 4
                , placeholder "Departure Time"
                , onInput UpdateDepartureTime
                ]
                []
            , label [ class "checkbox" ]
                [ text ""
                , input [ class "input", type_ "checkbox", onCheck UpdateDepartureIsNextUTCDay ] []
                ]
            , input [ class "input", type_ "text", placeholder "Gate", onInput UpdateGate ] []
            ]
        ]


viewCardSection : Model -> Html Msg
viewCardSection model =
    section [ class "section" ]
        [ div [ class "columns" ]
            [ viewHeaderCard ("FDX" ++ model.flightInfo.flightNumber) <| model.flightInfo.gate
            , viewLinkCard <| model.flightInfo.departureAirport
            , viewLinkCard <| model.flightInfo.arrivalAirport
            , viewClockCard model
            ]
        ]


viewClockCard : Model -> Html Msg
viewClockCard model =
    let
        --time = model.departureTime |> getTimeString
        timeToDeparture =
            getTimeToDeparture model.time model.flightInfo.depatureTotalMinutes model.flightInfo.departureIsNextUTCDay

        timeString =
            if model.flightInfo.departureIsNextUTCDay then
                (model.flightInfo.departureTime |> getValidTime |> getTimeString) ++ " Z +1"

            else
                (model.flightInfo.departureTime |> getValidTime |> getTimeString) ++ " Z"
    in
    div [ class "column" ]
        [ if timeToDeparture <= 0 then
            p [ class "title", style "color" "green" ] [ text ("T " ++ String.fromInt timeToDeparture) ]

          else
            p [ class "title", style "color" "red" ] [ text ("T+" ++ String.fromInt timeToDeparture) ]
        , p [ class "subtitle" ] [ text timeString ]
        ]


viewHeaderCard : String -> String -> Html Msg
viewHeaderCard title gate =
    div [ class "column" ]
        [ p [ class "title", onClick ShowEditor ] [ text title ]
        , p [ class "subtitle", onClick ShowEditor ] [ text ("Gate: " ++ gate) ]
        ]


viewLinkCard : String -> Html Msg
viewLinkCard title =
    let
        linkUrl =
            "https://www.aviationweather.gov/taf/board?ids=" ++ title ++ "&date=&submit=Goto+TAF+board"
    in
    div [ class "column" ]
        [ a [ class "title", href linkUrl, target "_blank" ] [ text title ]
        ]


viewCheckList : Model -> Html Msg
viewCheckList model =
    div [ class "section" ]
        [ -- div [ class "level"][
          -- p [ class "title is-2 level-left", classList [ ( "is-complete", getCompletedState "Weather" model.checkListItems ) ] ] [ text "Weather" ]
          -- , span [class "level-right"] [button [class "button is-success" ,onClick (CheckItem "Weather")] [text "X"]]
          -- ]
          {-
                 [( "WX", False )
             , ( "WX-TOA", False )
             , ( "WX-ALT", False )
             , ( "FP-CHK", False )
             , ( "CLR", False )
             , ( "STD-FXP", False )
             , ( "FXP-1", False )
             , ( "FXP-2", False )
             , ( "FXP-3", False )
             , ( "FXP-4", False )
             , ( "FXP-5", False )
             , ( "FXP-6", False )
             , ( "FXP-7", False )
             , ( "SEC", False )
             , ( "SEC-1", False )
             , ( "SEC-2", False )
             , ( "SEC-3", False )
             , ( "SEC-4", False )
             , ( "FOM", False )
             , ( "FOM-1", False )
             , ( "FOM-2", False )
             , ( "FOM-3", False )
             , ( "FOM-4", False )
             , ( "FOM-5", False )
             , ( "FOM-6", False )
             , ( "FOM-7", False )
             , ( "DEP", False )
             , ( "DEP-1", False )
             , ( "DEP-2", False )
             , ( "PER", False )
             , ( "BFS", False )
             , ( "FNL", False )
             , ( "APU", False )
          -}
          div [ class "level" ]
            [ span [ class "level-item level-right title is-6" ]
                [ text
                    (String.fromInt (getTotalCheckedItems model.checkListItems) ++ " of " ++ String.fromInt (getTotalCheckListItems model.checkListItems))
                ]
            , span [ class "level-item level-right" ]
                [ button [ class "button is-warning is-small", onClick ResetCheckList ] [ text "Reset" ]
                ]
            ]
        , div [ class "checklist" ]
            [ viewCheckListItemHelper "WX" "Weather" False model
            , viewCheckListItemHelper "WX-TOA" "Takeoff Alternate" True model
            , viewCheckListItemHelper "WX-ALT" "Alternate" True model
            , viewCheckListItemHelper "FP-CHK" "Flight Plan Check" False model
            , viewCheckListItemHelper "STD-FXP" "Standard FX Procedures" False model
            , viewCheckListItemHelper "FXP-1" "LTAET" True model
            , viewCheckListItemHelper "FXP-2" "Standard Callouts" True model
            , viewCheckListItemHelper "FXP-3" "Sterile Cockpit" True model
            , viewCheckListItemHelper "FXP-4" "Auto throttle/Auto Brakes" True model
            , viewCheckListItemHelper "FXP-5" "Autopilot" True model
            , viewCheckListItemHelper "FXP-6" "Rejected T/O" True model
            , viewCheckListItemHelper "FXP-7" "Emergency Return" True model
            , viewCheckListItemHelper "SEC" "Security" False model
            , viewCheckListItemHelper "SEC-1" "Ops Security" True model
            , viewCheckListItemHelper "SEC-2" "Law Enforcement" True model
            , viewCheckListItemHelper "SEC-3" "Cockpit Security" True model
            , viewCheckListItemHelper "SEC-4" "Document Security" True model
            , viewCheckListItemHelper "FOM" "FOM Briefing" False model
            , viewCheckListItemHelper "FOM-1" "PF" True model
            , viewCheckListItemHelper "FOM-2" "Company Jepp Pages" True model
            , viewCheckListItemHelper "FOM-3" "Specail Procedures/NOTAMS" True model
            , viewCheckListItemHelper "FOM-4" "MEL/CDL" True model
            , viewCheckListItemHelper "FOM-5" "MTOB" True model
            , viewCheckListItemHelper "FOM-6" "Other Abnormals" True model
            , viewCheckListItemHelper "FOM-7" "Threats" True model
            , viewCheckListItemHelper "CLR" "Clearance" False model
            , viewCheckListItemHelper "DEP" "Departure Brief" False model
            , viewCheckListItemHelper "DEP-1" "E/O Procedure" True model
            , viewCheckListItemHelper "DEP-2" "SID/Gradient" True model
            , viewCheckListItemHelper "PER" "Performance Brief" False model
            , viewCheckListItemHelper "BFS" "Before Start Checklist" False model
            , viewCheckListItemHelper "FNL" "Final Wt & Bal" False model
            , viewCheckListItemHelper "APU" "APU" False model
            ]
        , div [ class "level" ]
            [ span [ class "level-item has-text-centered title is-6" ]
                [ text
                    (String.fromInt (getTotalCheckedItems model.checkListItems) ++ " of " ++ String.fromInt (getTotalCheckListItems model.checkListItems))
                ]
            ]
        ]


getTotalCheckListItems : Dict String Bool -> Int
getTotalCheckListItems dict =
    dict |> Dict.toList |> List.length


getTotalCheckedItems : Dict String Bool -> Int
getTotalCheckedItems dict =
    dict |> Dict.toList |> List.filter (\( _, value ) -> value == True) |> List.length


viewCheckListItemHelper : String -> String -> Bool -> Model -> Html Msg
viewCheckListItemHelper key description isSubItem model =
    viewCheckListItem key description isSubItem (getCompletedState key model.checkListItems)


viewCheckListItem : String -> String -> Bool -> Bool -> Html Msg
viewCheckListItem key item isSubItem isComplete =
    div [ class "level mb-1", classList [ ( "is-complete", isComplete ) ] ]
        [ div
            [ class "title mb-2 level-left level-item"
            , classList
                [ ( "is-5", isSubItem )
                , ( "is-3", not isSubItem )
                ]
            ]
            [ text item ]
        , div [ class "level-right level-item" ] [ button [ class "button is-small ", onClick (CheckItem key) ] [ text "x" ] ]
        ]


getCompletedState : String -> Dict String Bool -> Bool
getCompletedState key dictionary =
    dictionary |> Dict.get key |> Maybe.withDefault False


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
    | CheckItem String
    | ResetCheckList


port setStorage : E.Value -> Cmd msg


port saveDict : E.Value -> Cmd msg



-- We want to `setStorage` on every update, so this function adds
-- the setStorage command on each step of the update function.
--
-- Check out index.html to see how this is handled on the JS side.
--


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel.flightInfo), saveDict (encodeDict newModel.checkListItems), cmds ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowEditor ->
            ( { model | isEditorVisible = True }, Cmd.none )

        HideEditor ->
            ( { model | isEditorVisible = False }, Cmd.none )

        UpdateFlightNumber flightNumber ->
            let
                fi =
                    model.flightInfo

                newfi =
                    { fi | flightNumber = flightNumber }
            in
            ( { model | flightInfo = newfi }, Cmd.none )

        UpdateDepartureAirport departureAirport ->
            let
                fi =
                    model.flightInfo

                newfi =
                    { fi | departureAirport = String.toUpper departureAirport }
            in
            ( { model | flightInfo = newfi }, Cmd.none )

        UpdateArrivalAirport arrivalAirport ->
            let
                fi =
                    model.flightInfo

                newfi =
                    { fi | arrivalAirport = String.toUpper arrivalAirport }
            in
            ( { model | flightInfo = newfi }, Cmd.none )

        UpdateDepartureTime departureTime ->
            let
                fi =
                    model.flightInfo

                newfi =
                    { fi
                        | departureTime = departureTime
                        , depatureTotalMinutes = departureTime |> getValidTime |> getTotalMinutes
                    }
            in
            ( { model | flightInfo = newfi }, Cmd.none )

        UpdateGate gate ->
            let
                fi =
                    model.flightInfo

                newfi =
                    { fi | gate = gate }
            in
            ( { model | flightInfo = newfi }, Cmd.none )

        UpdateDepartureIsNextUTCDay isNextDay ->
            let
                fi =
                    model.flightInfo

                newfi =
                    { fi | departureIsNextUTCDay = isNextDay }
            in
            ( { model | flightInfo = newfi }, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        CheckItem item ->
            let
                newDict =
                    Dict.update item (Maybe.map not) model.checkListItems
            in
            ( { model | checkListItems = newDict }, Cmd.none )

        ResetCheckList ->
            ( { model | checkListItems = initCheckList }, Cmd.none )


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decodeLocalStrorage flags of
        Ok localStorage ->
            let
                flightInfo =
                    localStorage.flightInfo

                newfi =
                    { flightInfo | depatureTotalMinutes = flightInfo.departureTime |> getValidTime |> getTotalMinutes }
            in
            { flightInfo = newfi
            , checkListItems = localStorage.checkListItems
            , isEditorVisible = False
            , time = localStorage.currentTime |> Time.millisToPosix
            }

        Err _ ->
            { flightInfo =
                { flightNumber = "0000"
                , departureAirport = "KMEM"
                , arrivalAirport = "KMEM"
                , departureTime = ( 0, 0 ) |> getTimeString
                , depatureTotalMinutes = 0
                , departureIsNextUTCDay = False
                , gate = "UNKN"
                }
            , checkListItems = initCheckList
            , isEditorVisible = False
            , time = Time.millisToPosix 0
            }
    , Cmd.none
    )


initCheckList : Dict String Bool
initCheckList =
    Dict.fromList
        [ ( "WX", False )
        , ( "WX-TOA", False )
        , ( "WX-ALT", False )
        , ( "FP-CHK", False )
        , ( "CLR", False )
        , ( "STD-FXP", False )
        , ( "FXP-1", False )
        , ( "FXP-2", False )
        , ( "FXP-3", False )
        , ( "FXP-4", False )
        , ( "FXP-5", False )
        , ( "FXP-6", False )
        , ( "FXP-7", False )
        , ( "SEC", False )
        , ( "SEC-1", False )
        , ( "SEC-2", False )
        , ( "SEC-3", False )
        , ( "SEC-4", False )
        , ( "FOM", False )
        , ( "FOM-1", False )
        , ( "FOM-2", False )
        , ( "FOM-3", False )
        , ( "FOM-4", False )
        , ( "FOM-5", False )
        , ( "FOM-6", False )
        , ( "FOM-7", False )
        , ( "DEP", False )
        , ( "DEP-1", False )
        , ( "DEP-2", False )
        , ( "PER", False )
        , ( "BFS", False )
        , ( "FNL", False )
        , ( "APU", False )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (20 * 1000) Tick


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
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


validateHour : Int -> Maybe Int
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


getValidTime : String -> ( Int, Int )
getValidTime value =
    case validateTimeLength value of
        Just validTime ->
            case ( getHour validTime, getMinute validTime ) of
                ( Just h, Just m ) ->
                    ( h, m )

                _ ->
                    ( 0, 0 )

        Nothing ->
            ( 0, 0 )


getTotalMinutes : ( Int, Int ) -> Int
getTotalMinutes ( hour, minute ) =
    hour * 60 + minute


getTimeString : ( Int, Int ) -> String
getTimeString ( hour, minute ) =
    String.padLeft 2 '0' (String.fromInt hour) ++ String.padLeft 2 '0' (String.fromInt minute)


getTimeToDeparture : Time.Posix -> Int -> Bool -> Int
getTimeToDeparture currentTime departureTime isNextDay =
    let
        h =
            Time.toHour Time.utc currentTime

        m =
            Time.toMinute Time.utc currentTime

        currentTotalMinutes =
            getTotalMinutes ( h, m )
    in
    if isNextDay then
        currentTotalMinutes - (1440 + departureTime)

    else
        currentTotalMinutes - departureTime



---- JSON Decoders and Encoders


decodeLocalStrorage : D.Decoder LocalStorage
decodeLocalStrorage =
    D.map3 LocalStorage
        (D.field "currentTime" D.int)
        (D.field "flightInfo" decoder)
        (D.field "checkListItems" decodeDict)


encode : FlightInfo -> E.Value
encode fi =
    E.object
        [ ( "flightNumber", E.string fi.flightNumber )
        , ( "departureAirport", E.string fi.departureAirport )
        , ( "arrivalAirport", E.string fi.arrivalAirport )
        , ( "departureTime", E.string fi.departureTime )
        , ( "depatureTotalMinutes", E.int fi.depatureTotalMinutes )
        , ( "departureIsNextUTCDay", E.bool fi.departureIsNextUTCDay )
        , ( "gate", E.string fi.gate )
        ]


decoder : D.Decoder FlightInfo
decoder =
    D.map7 FlightInfo
        (D.field "flightNumber" D.string)
        (D.field "departureAirport" D.string)
        (D.field "arrivalAirport" D.string)
        (D.field "departureTime" D.string)
        (D.field "depatureTotalMinutes" D.int)
        (D.field "departureIsNextUTCDay" D.bool)
        (D.field "gate" D.string)


encodeDict : Dict String Bool -> E.Value
encodeDict dict =
    E.object
        (Dict.toList dict
            |> List.map (\( key, value ) -> ( key, E.bool value ))
        )


decodeDict : D.Decoder (Dict String Bool)
decodeDict =
    D.dict D.bool
