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
    , remaining : Float
    }


type State
    = NotStarted
    | GetReady Int
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
    ( { state = NotStarted, bpm = 30, remaining = 120 * 1000 }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Coordination"
    , body =
        [ E.layout
            [ Font.color foregroundColor
            , Background.color backgroundColor
            , Font.size 50
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
                , E.spacing 20
                ]
             <|
                case model.state of
                    Done ->
                        [ E.text "Done" ]

                    GetReady 0 ->
                        [ E.text "GO!" ]

                    GetReady x ->
                        [ E.text (String.fromInt x) ]

                    NotStarted ->
                        [ Input.button
                            buttonStyles
                            { onPress = Just (Start LegsAlternating), label = E.text "Ipsi / Contra" }
                        , Input.slider
                            sliderStyles
                            { onChange = AdjustTime
                            , label =
                                Input.labelAbove []
                                    (viewRemaining model.remaining)
                            , min = 60 * 1000
                            , max = 300 * 1000
                            , step = Just (30 * 1000)
                            , value = model.remaining
                            , thumb = Input.defaultThumb
                            }
                        , Input.slider
                            sliderStyles
                            { onChange = AdjustBpm
                            , label =
                                Input.labelAbove []
                                    (E.text
                                        ("Bpm: "
                                            ++ String.fromFloat model.bpm
                                        )
                                    )
                            , min = 30
                            , max = 60
                            , step = Just 5
                            , value = model.bpm
                            , thumb = Input.defaultThumb
                            }
                        ]

                    Started game ->
                        [ viewRemaining model.remaining
                        , E.text (String.fromFloat model.bpm ++ " bpm")
                        , Input.button
                            buttonStyles
                            { onPress = Just (Pause game), label = E.text "Pause" }
                        ]

                    Paused game ->
                        [ viewRemaining model.remaining
                        , Input.button
                            buttonStyles
                            { onPress = Just (Start game), label = E.text "Continue" }
                        ]
            )
        ]
    }


viewRemaining : Float -> E.Element Msg
viewRemaining remaining =
    E.text (String.fromFloat (remaining / 1000) ++ "s")


sliderStyles =
    [ E.behindContent
        (E.el
            [ E.width E.fill
            , E.height (E.px 2)
            , E.centerY
            , Background.color foregroundColor
            , Border.rounded 2
            ]
            E.none
        )
    ]


buttonStyles =
    [ E.padding 10
    , Background.color foregroundColor
    , Font.color backgroundColor
    , Border.color foregroundColor
    , Border.width 1
    , Border.shadow { blur = 2, color = foregroundColor, offset = ( 0, 0 ), size = 1 }
    , Border.rounded 2
    ]


type Msg
    = NoOp
    | Start Game
    | Pause Game
    | Bpm
    | TickSeconds
    | NextCommand Commands
    | AdjustTime Float
    | AdjustBpm Float
    | StartOver


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartOver ->
            ( { model | state = NotStarted }, Cmd.none )

        AdjustTime minutes ->
            ( { model | remaining = minutes }, Cmd.none )

        AdjustBpm bpm ->
            ( { model | bpm = bpm }, Cmd.none )

        Start game ->
            let
                getReady x =
                    Cmd.batch
                        [ Process.sleep (60 * 1000 / model.bpm)
                            |> Task.perform (\_ -> Start game)
                        , speak
                            (if x == 0 then
                                "go"

                             else
                                String.fromInt x
                            )
                        ]
            in
            case model.state of
                NotStarted ->
                    ( { model | state = GetReady 3 }, getReady 3 )

                GetReady 0 ->
                    ( { model | state = Started game }
                    , Random.generate NextCommand ipsiContra
                    )

                GetReady x ->
                    ( { model | state = GetReady (x - 1) }
                    , getReady (x - 1)
                    )

                Paused _ ->
                    ( { model | state = GetReady 3 }, getReady 3 )

                _ ->
                    ( model, Cmd.none )

        TickSeconds ->
            if model.remaining - 1000 <= 0 then
                ( { model | remaining = 0, state = Done }
                , Cmd.batch
                    [ Process.sleep (5 * 1000)
                        |> Task.perform (\_ -> StartOver)
                    , speak "Done"
                    ]
                )

            else
                ( { model | remaining = model.remaining - 1000 }
                , Cmd.none
                )

        Bpm ->
            ( model
            , Random.generate NextCommand ipsiContra
            )

        NextCommand command ->
            ( model
            , case model.state of
                Started game ->
                    speak (commandToString command)

                _ ->
                    Cmd.none
            )

        Pause game ->
            ( { model | state = Paused game }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { state, bpm } =
    case state of
        Started _ ->
            Sub.batch
                [ Time.every (60 * 1000 / bpm) (\_ -> Bpm)
                , Time.every 1000 (\_ -> TickSeconds)
                ]

        _ ->
            Sub.none


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
