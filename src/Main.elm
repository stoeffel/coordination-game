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
    , game : Game
    , bpm : Float
    , remaining : Float
    , rest : Float
    }


type State
    = NotStarted
    | GetReady Int
    | Started
    | Paused
    | Done


type Game
    = LegsAlternating
    | Rest


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
        { init = \_ _ _ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


port speak : String -> Cmd msg


type alias Flags =
    {}


init : Model
init =
    { state = NotStarted
    , game = Rest
    , bpm = 30
    , remaining = minutes 2
    , rest = minutes 1
    }


seconds : Float -> Float
seconds x =
    x * 1000


minutes : Float -> Float
minutes x =
    seconds (60 * x)


view : Model -> Browser.Document Msg
view model =
    { title = "Coordination"
    , body =
        [ E.layout
            [ Font.color foregroundColor
            , Background.color backgroundColor
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
                (case model.state of
                    Done ->
                        [ h2 "Done" ]

                    GetReady 0 ->
                        [ h2 "GO!" ]

                    GetReady x ->
                        [ h2 (String.fromInt x) ]

                    NotStarted ->
                        [ h2 "Ipsi / Contra"
                        , Input.slider
                            sliderStyles
                            { onChange = AdjustTime
                            , label =
                                Input.labelAbove []
                                    (viewTime Normal model.remaining)
                            , min = minutes 1
                            , max = minutes 5
                            , step = Just (seconds 30)
                            , value = model.remaining
                            , thumb = Input.defaultThumb
                            }
                        , Input.slider
                            sliderStyles
                            { onChange = AdjustBpm
                            , label =
                                Input.labelAbove []
                                    (viewBpm model.bpm)
                            , min = 30
                            , max = 60
                            , step = Just 5
                            , value = model.bpm
                            , thumb = Input.defaultThumb
                            }
                        , Input.button
                            buttonStyles
                            { onPress = Just (Start LegsAlternating), label = E.text "Start" }
                        , h2 "Rest"
                        , Input.slider
                            sliderStyles
                            { onChange = AdjustRest
                            , label =
                                Input.labelAbove []
                                    (viewTime Normal model.rest)
                            , min = minutes 1
                            , max = minutes 5
                            , step = Just (seconds 30)
                            , value = model.rest
                            , thumb = Input.defaultThumb
                            }
                        , Input.button
                            buttonStyles
                            { onPress = Just (Start Rest), label = E.text "Start" }
                        ]

                    Started ->
                        [ viewTime Big
                            (case model.game of
                                Rest ->
                                    model.rest

                                LegsAlternating ->
                                    model.remaining
                            )
                        , case model.game of
                            Rest ->
                                E.none

                            LegsAlternating ->
                                viewBpm model.bpm
                        , Input.button
                            buttonStyles
                            { onPress = Just Pause, label = E.text "Pause" }
                        ]

                    Paused ->
                        [ viewTime Big
                            (case model.game of
                                Rest ->
                                    model.rest

                                LegsAlternating ->
                                    model.remaining
                            )
                        , case model.game of
                            Rest ->
                                E.none

                            LegsAlternating ->
                                viewBpm model.bpm
                        , E.row [ E.spacing 20, E.centerX ]
                            [ Input.button
                                buttonStyles
                                { onPress = Just (Start model.game), label = E.text "Continue" }
                            , Input.button
                                buttonStyles
                                { onPress = Just StartOver, label = E.text "Quit" }
                            ]
                        ]
                )
            )
        ]
    }


h2 : String -> E.Element msg
h2 text =
    E.html (Html.h2 [ Attr.style "font-size" "2em" ] [ Html.text text ])


type Size
    = Normal
    | Big


viewTime : Size -> Float -> E.Element msg
viewTime size remaining =
    (String.fromFloat (remaining / seconds 1) ++ "s")
        |> E.text
        |> E.el [ E.centerX, sizeToStyle size ]


viewBpm : Float -> E.Element msg
viewBpm bpm =
    (String.fromFloat bpm ++ " bpm")
        |> E.text
        |> E.el [ E.centerX, sizeToStyle Normal ]


sizeToStyle : Size -> E.Attribute msg
sizeToStyle size =
    case size of
        Normal ->
            Font.size (scaled 4)

        Big ->
            Font.size (scaled 10)


scaled : Int -> Int
scaled x =
    E.modular 16 1.25 x
        |> ceiling


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
    , E.centerX
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
    | Pause
    | Bpm
    | TickSeconds
    | NextCommand Commands
    | AdjustTime Float
    | AdjustRest Float
    | AdjustBpm Float
    | StartOver


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartOver ->
            ( { init | bpm = model.bpm }, Cmd.none )

        AdjustTime remaining ->
            ( { model | remaining = remaining }, Cmd.none )

        AdjustRest rest ->
            ( { model | rest = rest }, Cmd.none )

        AdjustBpm bpm ->
            ( { model | bpm = bpm }, Cmd.none )

        Start Rest ->
            ( { model | state = Started, game = Rest }, Cmd.none )

        Start LegsAlternating ->
            let
                getReady x =
                    Cmd.batch
                        [ Process.sleep (minutes 1 / model.bpm)
                            |> Task.perform (\_ -> Start LegsAlternating)
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
                    ( { model | state = Started, game = LegsAlternating }
                    , Random.generate NextCommand ipsiContra
                    )

                GetReady x ->
                    ( { model | state = GetReady (x - 1) }
                    , getReady (x - 1)
                    )

                Paused ->
                    ( { model | state = GetReady 3 }, getReady 3 )

                _ ->
                    ( model, Cmd.none )

        TickSeconds ->
            let
                ( time, setTime ) =
                    case model.game of
                        Rest ->
                            ( model.rest, \rest -> { model | rest = rest } )

                        LegsAlternating ->
                            ( model.remaining, \remaining -> { model | remaining = remaining } )
            in
            if time - seconds 1 <= 0 then
                ( setTime 0
                    |> (\m -> { m | state = Done })
                , Cmd.batch
                    [ Process.sleep (seconds 1)
                        |> Task.perform (\_ -> StartOver)
                    , speak "Done"
                    ]
                )

            else
                ( setTime (time - seconds 1), Cmd.none )

        Bpm ->
            ( model
            , Random.generate NextCommand ipsiContra
            )

        NextCommand command ->
            ( model
            , case model.game of
                Rest ->
                    Cmd.none

                LegsAlternating ->
                    speak (commandToString command)
            )

        Pause ->
            ( { model | state = Paused }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { state, bpm } =
    case state of
        Started ->
            Sub.batch
                [ Time.every (minutes 1 / bpm) (\_ -> Bpm)
                , Time.every (seconds 1) (\_ -> TickSeconds)
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
