port module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Color exposing (Color)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as EK
import Html
import Html.Attributes as Attr
import Json.Decode as D
import Process
import Random
import Task
import Time
import Url exposing (Url)


type alias Model =
    { state : State
    , bpm : Float
    , timer : Float
    , remaining : Float
    }


type State
    = NotStarted
    | Started Game
    | Paused Game
    | Done


type Game
    = LegsAlternating


type Commands
    = Ipsi
    | Contra


ipsiContra : Random.Generator Commands
ipsiContra =
    Random.uniform Ipsi [ Contra ]


commandToString : Commands -> String
commandToString command =
    case command of
        Ipsi ->
            "ypsi"

        Contra ->
            "contra"


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


port speak : String -> Cmd msg


type alias Flags =
    {}


init : D.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { state = NotStarted, bpm = 40, timer = 120, remaining = 120 }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Coordination"
    , body =
        [ E.layout
            [ Font.color foregroundColor
            , Background.color backgroundColor
            , Font.size 30
            , Font.family
                [ Font.typeface "Patrick Hand"
                , Font.sansSerif
                ]
            , E.height E.fill
            , E.width E.fill
            ]
            (E.column
                [ E.centerX
                , E.centerY
                , E.spaceEvenly
                ]
             <|
                case model.state of
                    Done ->
                        [ E.text "Done" ]

                    NotStarted ->
                        [ Input.button
                            [ E.centerX
                            , E.centerY
                            , E.padding 10
                            , Background.color foregroundColor
                            , Font.color backgroundColor
                            , Border.color foregroundColor
                            , Border.width 1
                            , Border.shadow { blur = 5, color = foregroundColor, offset = ( 0, 0 ), size = 1 }
                            , Border.rounded 8
                            ]
                            { onPress = Just (Start LegsAlternating), label = E.text "Ipsi / Contra - Legs Alternating" }
                        , Input.slider []
                            { onChange = AdjustTime
                            , label =
                                Input.labelAbove []
                                    (E.text
                                        ("Timer: "
                                            ++ String.fromFloat model.timer
                                            ++ "s"
                                        )
                                    )
                            , min = 60
                            , max = 300
                            , step = Just 30
                            , value = model.timer
                            , thumb = Input.defaultThumb
                            }
                        ]

                    Started game ->
                        [ E.text (String.fromFloat model.remaining ++ "s")
                        , E.text (String.fromFloat model.bpm ++ " bpm")
                        , Input.button
                            [ E.centerX
                            , E.centerY
                            , E.padding 10
                            , Background.color foregroundColor
                            , Font.color backgroundColor
                            , Border.color foregroundColor
                            , Border.width 1
                            , Border.shadow { blur = 5, color = foregroundColor, offset = ( 0, 0 ), size = 1 }
                            , Border.rounded 8
                            ]
                            { onPress = Just (Pause game), label = E.text "Pause" }
                        ]

                    Paused game ->
                        [ E.text (String.fromFloat model.remaining ++ "s")
                        , Input.button
                            [ E.centerX
                            , E.centerY
                            , E.padding 10
                            , Background.color foregroundColor
                            , Font.color backgroundColor
                            , Border.color foregroundColor
                            , Border.width 1
                            , Border.shadow { blur = 5, color = foregroundColor, offset = ( 0, 0 ), size = 1 }
                            , Border.rounded 8
                            ]
                            { onPress = Just (Continue game), label = E.text "Continue" }
                        ]
            )
        ]
    }


type Msg
    = NoOp
    | Start Game
    | Continue Game
    | Pause Game
    | Bpm
    | NextCommand Commands
    | AdjustTime Float
    | TickSeconds
    | StartOver


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartOver ->
            ( { model | state = NotStarted }, Cmd.none )

        AdjustTime minutes ->
            ( { model | timer = minutes, remaining = minutes }, Cmd.none )

        Start game ->
            ( { model | state = Started game }
            , Random.generate NextCommand ipsiContra
            )

        TickSeconds ->
            case model.state of
                Started game ->
                    let
                        newRemaining =
                            model.remaining - 1

                        steps =
                            model.timer / 5

                        newBpm =
                            if newRemaining < model.timer - 4 * steps then
                                60

                            else if newRemaining < model.timer - 3 * steps then
                                55

                            else if newRemaining < model.timer - 2 * steps then
                                50

                            else if newRemaining < model.timer - steps then
                                45

                            else
                                40
                    in
                    ( { model
                        | remaining = newRemaining
                        , bpm = newBpm
                        , state =
                            if newRemaining <= 0 then
                                Done

                            else
                                model.state
                      }
                    , if newRemaining <= 0 then
                        Cmd.batch
                            [ Process.sleep (5 * 1000)
                                |> Task.perform (\_ -> StartOver)
                            , speak "Done"
                            ]

                      else
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Bpm ->
            ( model, Random.generate NextCommand ipsiContra )

        NextCommand command ->
            case model.state of
                Started _ ->
                    ( model, speak (commandToString command) )

                _ ->
                    ( model, Cmd.none )

        Pause game ->
            ( { model | state = Paused game }, Cmd.none )

        Continue game ->
            ( { model | state = Started game }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { timer, bpm } =
    Sub.batch
        [ Time.every (60 * 1000 / bpm) (\_ -> Bpm)
        , Time.every 1000 (\_ -> TickSeconds)
        ]


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp


foregroundColor : E.Color
foregroundColor =
    E.rgb255 38 70 83


backgroundColor : E.Color
backgroundColor =
    E.rgb 0.89 0.89 0.89
