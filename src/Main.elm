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
import Random.Extra
import Task
import Time
import Url exposing (Url)
import Widget
import Widget.Style.Material as Material


type alias Model =
    { state : State
    , game : Game
    , bpm : Float
    , everyX : Float
    , remaining : Float
    , rest : Float
    , previousCmd : Maybe Command
    }


gameFromInt : Int -> Maybe Game
gameFromInt x =
    case x of
        0 ->
            Just Isolated

        1 ->
            Just AllDirections

        _ ->
            Nothing


gameToInt : Game -> Maybe Int
gameToInt x =
    case x of
        Isolated ->
            Just 0

        AllDirections ->
            Just 1

        _ ->
            Nothing


type State
    = NotStarted
    | GetReady Int
    | Started
    | Paused
    | Done


type Game
    = Isolated
    | AllDirections
    | Rest


type Command
    = Ipsi
    | Contra
    | Outside
    | Inside
    | Forward
    | Backward
    | Combined Command Command


ipsiContraGen : Random.Generator Command
ipsiContraGen =
    Random.uniform Ipsi [ Contra ]


allDirectionsGen : Command -> Random.Generator Command
allDirectionsGen prev =
    combinedGen
        |> Random.andThen
            (\c ->
                Random.weighted ( 0.4, c )
                    [ ( 0.1, Ipsi )
                    , ( 0.1, Contra )
                    , ( 0.1, Outside )
                    , ( 0.1, Inside )
                    , ( 0.1, Forward )
                    , ( 0.1, Backward )
                    ]
            )
        |> Random.Extra.filter
            (\c ->
                case prev of
                    Combined x y ->
                        c /= x && c /= y && c /= prev

                    x ->
                        c /= x
            )


combinedGen : Random.Generator Command
combinedGen =
    Random.uniform Outside [ Inside, Forward, Backward ]
        |> Random.andThen
            (\first ->
                Random.uniform Ipsi [ Contra ]
                    |> Random.map (Combined first)
            )


commandToString : Command -> String
commandToString command =
    case command of
        Ipsi ->
            "ypsi"

        Contra ->
            "contra"

        Outside ->
            "outside"

        Inside ->
            "inside"

        Forward ->
            "forward"

        Backward ->
            "backward"

        Combined first second ->
            commandToString first ++ " " ++ commandToString second


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
    , game = Isolated
    , bpm = 60
    , everyX = 5
    , remaining = minutes 2
    , rest = minutes 1
    , previousCmd = Nothing
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
            , E.paddingXY 0 50
            ]
            (case model.state of
                Done ->
                    E.column [ E.centerX, E.centerY ] [ h2 "Done" ]

                GetReady 0 ->
                    E.column [ E.centerX, E.centerY ] [ h2 "GO!" ]

                GetReady x ->
                    E.column [ E.centerX, E.centerY ] [ h2 (String.fromInt x) ]

                NotStarted ->
                    Widget.tab (Material.tab Material.defaultPalette)
                        { tabs =
                            { selected = gameToInt model.game
                            , options =
                                [ { text = "Isolated"
                                  , icon = E.none
                                  }
                                , { text = "All Directions"
                                  , icon = E.none
                                  }
                                ]
                            , onSelect = gameFromInt >> Maybe.map ChangedTab
                            }
                        , content =
                            \type_ ->
                                case Maybe.andThen gameFromInt type_ of
                                    Just Isolated ->
                                        viewIsolated model

                                    Just AllDirections ->
                                        viewAllDirections model

                                    _ ->
                                        E.none
                        }

                Started ->
                    E.column
                        [ E.centerX
                        , E.centerY
                        , E.spacing 20
                        ]
                        [ viewTime Big
                            (case model.game of
                                Rest ->
                                    model.rest

                                _ ->
                                    model.remaining
                            )
                        , case model.game of
                            Rest ->
                                E.none

                            Isolated ->
                                viewBpm model.bpm

                            AllDirections ->
                                E.none
                        , Input.button
                            buttonStyles
                            { onPress = Just Pause, label = E.text "Pause" }
                        ]

                Paused ->
                    E.column
                        [ E.centerX
                        , E.centerY
                        , E.spacing 20
                        ]
                        [ viewTime Big
                            (case model.game of
                                Rest ->
                                    model.rest

                                _ ->
                                    model.remaining
                            )
                        , case model.game of
                            Rest ->
                                E.none

                            Isolated ->
                                viewBpm model.bpm

                            AllDirections ->
                                E.none
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
        ]
    }


viewAllDirections : Model -> E.Element Msg
viewAllDirections model =
    E.column
        [ E.width E.fill
        , E.height E.fill
        , E.spacing 20
        , E.padding 50
        ]
        [ viewAdjustTime model
        , Input.slider
            sliderStyles
            { onChange = AdjustEveryX
            , label =
                Input.labelAbove []
                    (viewEveryX model.everyX)
            , min = 1
            , max = 8
            , step = Just 1
            , value = model.everyX
            , thumb = Input.defaultThumb
            }
        , Input.button
            buttonStyles
            { onPress = Just (Start AllDirections), label = E.text "Start" }
        , viewRest model
        ]


viewIsolated : Model -> E.Element Msg
viewIsolated model =
    E.column
        [ E.width E.fill
        , E.height E.fill
        , E.spacing 20
        , E.padding 50
        ]
        [ viewAdjustTime model
        , Input.slider
            sliderStyles
            { onChange = AdjustBpm
            , label =
                Input.labelAbove []
                    (viewBpm model.bpm)
            , min = 30
            , max = 90
            , step = Just 5
            , value = model.bpm
            , thumb = Input.defaultThumb
            }
        , Input.button
            buttonStyles
            { onPress = Just (Start Isolated), label = E.text "Start" }
        , viewRest model
        ]


viewAdjustTime : Model -> E.Element Msg
viewAdjustTime model =
    Input.slider
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


viewRest : Model -> E.Element Msg
viewRest model =
    E.column [ E.width E.fill ]
        [ h2 "Rest"
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


viewEveryX : Float -> E.Element msg
viewEveryX n =
    ("Every " ++ String.fromFloat n ++ "s")
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
    | TriggerCommand
    | TickSeconds
    | NextCommand Command
    | AdjustTime Float
    | AdjustRest Float
    | AdjustBpm Float
    | AdjustEveryX Float
    | StartOver
    | ChangedTab Game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangedTab game ->
            ( { model | game = game }, Cmd.none )

        StartOver ->
            ( { init | bpm = model.bpm, game = model.game }, Cmd.none )

        AdjustTime remaining ->
            ( { model | remaining = remaining }, Cmd.none )

        AdjustRest rest ->
            ( { model | rest = rest }, Cmd.none )

        AdjustBpm bpm ->
            ( { model | bpm = bpm }, Cmd.none )

        AdjustEveryX everyX ->
            ( { model | everyX = everyX }, Cmd.none )

        Start Rest ->
            ( { model | state = Started, game = Rest }, Cmd.none )

        Start game ->
            let
                getReady x =
                    Cmd.batch
                        [ Process.sleep (minutes 1 / model.bpm)
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
                    ( { model | state = Started, game = game }
                    , Random.generate NextCommand <|
                        case model.game of
                            AllDirections ->
                                combinedGen

                            _ ->
                                ipsiContraGen
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

                        Isolated ->
                            ( model.remaining, \remaining -> { model | remaining = remaining } )

                        AllDirections ->
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

        TriggerCommand ->
            ( model
            , Random.generate NextCommand <|
                case model.game of
                    AllDirections ->
                        case model.previousCmd of
                            Just prev ->
                                allDirectionsGen prev

                            Nothing ->
                                combinedGen

                    _ ->
                        ipsiContraGen
            )

        NextCommand command ->
            ( { model | previousCmd = Just command }
            , case model.game of
                Rest ->
                    Cmd.none

                _ ->
                    speak (commandToString command)
            )

        Pause ->
            ( { model | state = Paused }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { state, bpm, game, everyX } =
    case state of
        Started ->
            Sub.batch
                [ case game of
                    AllDirections ->
                        Time.every (seconds everyX) (\_ -> TriggerCommand)

                    _ ->
                        Time.every (minutes 1 / bpm) (\_ -> TriggerCommand)
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
