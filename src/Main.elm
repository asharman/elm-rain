module Main exposing (Model, Msg, main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Canvas.Settings as Canvas
import Color
import Constants exposing (WorldInfo)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import List
import Raindrop exposing (Raindrop)
import Random


type alias State =
    { width : Float
    , height : Float
    , raindrops : List Raindrop
    , debug : Bool
    , numberOfDrops : Int
    , seed : Random.Seed
    , windDirection : Float
    , windNoise : Float -> Float -> Float -> Float -> Float
    , time : Float
    }


type Model
    = Playing State
    | Paused State


toState : Model -> State
toState model =
    case model of
        Playing state ->
            state

        Paused state ->
            state


updateState : Model -> State -> Model
updateState model =
    case model of
        Playing _ ->
            Playing

        Paused _ ->
            Paused


isPaused : Model -> Bool
isPaused model =
    case model of
        Playing _ ->
            False

        Paused _ ->
            True


initialModel : Float -> Float -> Model
initialModel width height =
    Playing
        { width = width
        , height = height
        , raindrops = []
        , debug = False
        , numberOfDrops = 100
        , seed = Random.initialSeed 1
        , windDirection = 0
        , windNoise = Constants.windNoiseFromSeed 1
        , time = 0
        }


toWorldInfo : Model -> WorldInfo
toWorldInfo model =
    let
        state =
            toState model

        windAtPosition =
            \x y z ->
                state.windNoise state.time x y z
                    |> (*) (2 * pi)
                    |> (\turn -> ( cos turn, sin turn ))
    in
    { canvasHeight = state.height
    , canvasWidth = state.width
    , randomSeed = state.seed
    , windDirection = state.windDirection
    , gravity = Constants.gravity
    , windAtPosition = windAtPosition
    , debug = state.debug
    }


type Msg
    = Frame Float
    | BrowserResized Int Int
    | DebugChecked Bool
    | PauseChecked Bool
    | WindChanged Float
    | NumberOfDropsChanged Int
    | GeneratedDrops (List Raindrop)


main : Program ( Float, Float ) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta Frame
                    , onResize BrowserResized
                    ]
        }


init : ( Float, Float ) -> ( Model, Cmd Msg )
init ( width, height ) =
    let
        model =
            initialModel width height

        state =
            toState model
    in
    ( model
    , Random.generate GeneratedDrops
        (Random.list state.numberOfDrops (Raindrop.randomRainDrop (toWorldInfo model)))
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        internalState =
            toState model
    in
    case ( model, msg ) of
        ( Playing state, Frame deltaTime ) ->
            let
                ( newDrops, newSeed ) =
                    List.foldl
                        (\drop ( acc, oldSeed ) ->
                            drop
                                |> Raindrop.update deltaTime (toWorldInfo model) oldSeed
                                |> Tuple.mapFirst (List.singleton >> List.append acc)
                        )
                        ( [], state.seed )
                        (List.filter
                            (\drop ->
                                (List.length state.raindrops <= state.numberOfDrops)
                                    || not (Raindrop.isRaindropOffScreen state.height drop)
                            )
                            state.raindrops
                        )
            in
            ( Playing
                { state
                    | raindrops = newDrops
                    , seed = newSeed
                    , time = state.time + deltaTime
                }
            , Cmd.none
            )

        ( Paused _, Frame _ ) ->
            ( model, Cmd.none )

        ( _, BrowserResized w h ) ->
            let
                width =
                    toFloat w

                height =
                    toFloat h
            in
            ( updateState model
                { internalState
                    | width = width
                    , height = height
                }
            , Cmd.none
            )

        ( _, DebugChecked value ) ->
            ( updateState model { internalState | debug = value }, Cmd.none )

        ( _, PauseChecked paused ) ->
            if paused then
                ( Paused internalState, Cmd.none )

            else
                ( Playing internalState, Cmd.none )

        ( _, WindChanged direction ) ->
            ( updateState model { internalState | windDirection = direction }, Cmd.none )

        ( _, NumberOfDropsChanged numOfDrops ) ->
            let
                numberToGenerate =
                    max 0 numOfDrops - List.length internalState.raindrops
            in
            ( updateState model { internalState | numberOfDrops = numOfDrops }
            , Random.generate GeneratedDrops
                (Random.list numberToGenerate
                    (Raindrop.randomRainDrop (toWorldInfo model))
                )
            )

        ( _, GeneratedDrops newDrops ) ->
            ( updateState model { internalState | raindrops = newDrops ++ internalState.raindrops }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        state =
            toState model
    in
    Html.div
        [ Attributes.style "position" "relative"
        , Attributes.style "padding" "0"
        , Attributes.style "margin" "0"
        ]
        [ Html.div
            [ Attributes.style "display" "flex"
            , Attributes.style "justify-content" "center"
            , Attributes.style "align-items" "center"
            ]
            [ Canvas.toHtml
                ( round state.width, round state.height )
                []
                [ clearScreen state.width state.height
                , Canvas.group []
                    (List.map (Raindrop.render (toWorldInfo model)) state.raindrops)
                ]
            ]
        , Html.form
            [ Attributes.style "position" "absolute"
            , Attributes.style "bottom" "1rem"
            , Attributes.style "left" "1rem"
            , Attributes.class "stack"
            ]
            [ Html.label
                [ Attributes.style "display" "flex"
                , Attributes.style "gap" "0.5rem"
                ]
                [ Html.input
                    [ Attributes.type_ "checkbox"
                    , Attributes.style "padding" "0"
                    , Attributes.style "margin" "0"
                    , Attributes.checked state.debug
                    , Events.onCheck DebugChecked
                    ]
                    []
                , Html.text "Debug"
                ]
            , Html.label
                [ Attributes.style "display" "flex"
                , Attributes.style "gap" "0.5rem"
                ]
                [ Html.input
                    [ Attributes.type_ "checkbox"
                    , Attributes.style "padding" "0"
                    , Attributes.style "margin" "0"
                    , Attributes.checked <| isPaused model
                    , Events.onCheck PauseChecked
                    ]
                    []
                , Html.text "Pause"
                ]
            , Html.label
                [ Attributes.style "display" "flex"
                , Attributes.style "align-items" "center"
                , Attributes.style "gap" "0.5rem"
                ]
                [ Html.input
                    [ Attributes.type_ "range"
                    , Attributes.id "wind"
                    , Attributes.min "-1"
                    , Attributes.max "1"
                    , Attributes.step "0.01"
                    , Attributes.value <| String.fromFloat state.windDirection
                    , onRangeInput WindChanged
                    ]
                    []
                , Html.text "Wind Direction"
                ]
            , Html.label
                [ Attributes.style "display" "flex"
                , Attributes.style "align-items" "center"
                , Attributes.style "gap" "0.5rem"
                ]
                [ Html.input
                    [ Attributes.type_ "number"
                    , Attributes.max "100"
                    , Attributes.min "1"
                    , Attributes.value (String.fromInt state.numberOfDrops)
                    , onPositiveIntInput NumberOfDropsChanged
                    ]
                    []
                , Html.text "Raindrop Count"
                ]
            ]
        ]


onRangeInput : (Float -> Msg) -> Html.Attribute Msg
onRangeInput tagger =
    let
        floatDecoder =
            \val ->
                case String.toFloat val of
                    Just f ->
                        Decode.succeed f

                    Nothing ->
                        Decode.fail "Not an Integer"
    in
    Events.on "input" <|
        Decode.map tagger (Events.targetValue |> Decode.andThen floatDecoder)


onPositiveIntInput : (Int -> Msg) -> Html.Attribute Msg
onPositiveIntInput tagger =
    let
        intDecoder =
            \val ->
                case String.toInt val of
                    Just f ->
                        if f >= 0 then
                            Decode.succeed f

                        else
                            Decode.fail "Integer is negative"

                    Nothing ->
                        Decode.fail "Not an Integer"
    in
    Events.on "input" <|
        Decode.map tagger (Events.targetValue |> Decode.andThen intDecoder)


clearScreen : Float -> Float -> Canvas.Renderable
clearScreen width height =
    Canvas.shapes [ Canvas.fill Color.black ]
        [ Canvas.rect ( 0, 0 ) width height ]
