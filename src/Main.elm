module Main exposing (..)

import Html as Html exposing (Html, div,p, label, input, h3, map)
import Html.Events exposing (onInput)
import Html.Attributes exposing (type_, value)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Browser
import Time


-- MAIN
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ = (initModel , Cmd.none)

initModel : Model
initModel = Setup ( { round_length_sec = 5, rest_length_sec = 0, rounds = 2})

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        Setup setupModel ->
            case msg of
                RoundTimeInputChange str ->
                    (Setup {setupModel | round_length_sec = toSeconds str setupModel.round_length_sec }, Cmd.none)
                RestInputChange str ->
                    (Setup {setupModel | rest_length_sec = toSeconds str setupModel.rest_length_sec }, Cmd.none)
                NumRoundsInputChange str ->
                    (Setup {setupModel | rounds = toSeconds str setupModel.rounds }, Cmd.none)
                Start -> (Running { current_round = setupModel.rounds
                                  , round_length_sec = setupModel.round_length_sec
                                  , rest_length_sec = setupModel.rest_length_sec
                                  , current_time = newWatchTime setupModel.round_length_sec
                                  , state = Active } , Cmd.none)
                _ -> (model, Cmd.none)
        Running info ->
            case msg of
                Tick -> (updateTick info, Cmd.none)
                _ -> (model, Cmd.none)


updateTick : RunInfo -> Model
updateTick info =
    case tickTime info.current_time of
        Just wt -> Running { info | current_time = wt}
        Nothing ->
            nextState info



nextState: RunInfo -> Model
nextState info =
    if info.current_round - 1 <= 0 then
        Running { info| state = Finished, current_time = newWatchTime 0, current_round = 0}
    else
        case info.state of
            Active ->
                if info.rest_length_sec > 0 then
                    Running { info | current_time = newWatchTime info.rest_length_sec, state = Rest}
                else
                    Running { info | current_time = newWatchTime info.round_length_sec, current_round = info.current_round - 1}
            Rest -> Running { info | current_time = newWatchTime info.round_length_sec, state = Active, current_round = info.current_round - 1}
            _ -> Running info



newWatchTime: Int -> WatchTime
newWatchTime sec =
    {sec = sec, centi_sec = 0}

tickTime : WatchTime -> Maybe WatchTime
tickTime wt =
    if wt.centi_sec - 1 <= 0 then
        let
            whole = wt.sec - 1
        in
            if whole < 0 then
                Nothing
            else
                Just { sec = whole, centi_sec = 100 }
    else
        Just { wt | centi_sec = wt.centi_sec - 1 }




toSeconds: String -> Int -> Int
toSeconds str current =
    case String.toInt str of
        Just num -> max 0 num
        Nothing -> current



-- MODEL AND TYPES

type Msg = RestInputChange String
         | RoundTimeInputChange String
         | NumRoundsInputChange String
         | Start
         | Tick

type Model = Setup SetupInfo
           | Running RunInfo


type State = Rest | Active | Finished

type alias SetupInfo = { round_length_sec : Int
                       , rest_length_sec : Int
                       , rounds: Int
                     }

type alias WatchTime = { sec : Int, centi_sec : Int}


type alias RunInfo = { current_round : Int
                     , round_length_sec : Int
                     , rest_length_sec : Int
                     , current_time : WatchTime
                     , state : State
                     }

-- LAYOUT
view : Model -> Html Msg
view model =
    Element.layout [] <|
        case model of
            Setup setupInfo -> viewSetup setupInfo
            Running runInfo -> viewRunning runInfo




viewRunning : RunInfo -> Element Msg
viewRunning info =
    column [centerX] [ text <| timeString info.current_time
                     , el [] <| text <| case info.state of
                                   Rest -> "Rest"
                                   Active -> "Work"
                                   Finished -> "Finished"
                     , text <| "RoundLeft:" ++ String.fromInt info.current_round
                     , Input.button [Background.color startColor] { label = text "Pause"
                                       , onPress = Nothing
                                       }
                     ]


timeString : WatchTime -> String
timeString wt =
    (String.fromInt wt.sec) ++ (":") ++ (String.fromInt wt.centi_sec)


viewSetup : SetupInfo -> Element Msg
viewSetup info =
    column [centerX] [ row [] [text "Seconds in Round:"
                              , inputButton info.round_length_sec RoundTimeInputChange
                           ]
                     , row [] [text "Seconds in Rest:"
                              , inputButton info.rest_length_sec RestInputChange
                           ]
                          , row [] [text "Number of rounds:"
                              , inputButton info.rounds NumRoundsInputChange
                           ]
                     , Input.button [Background.color startColor] { label = text "Start"
                                       , onPress = Just Start
                                       }

                     ]

startColor =
    Element.rgb255 238 238 238

inputButton : Int -> (String -> Msg) -> Element Msg
inputButton val f =
     html ( div [] [ input
                         [ type_ "number"
                         , value <| String.fromInt val
                         , onInput f ] []
                   ])


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running info ->
            Time.every 10 (\_ -> Tick)
        _ -> Sub.none
