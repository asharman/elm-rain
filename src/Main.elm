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
import Vector exposing (Vector)



-- CONSTANTS


type alias Model =
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


initialModel : Float -> Float -> Model
initialModel width height =
    { width = width
    , height = height
    , raindrops = []
    , debug = True
    , numberOfDrops = 10
    , seed = Random.initialSeed 1
    , windDirection = 0
    , windNoise = Constants.windNoiseFromSeed 1
    , time = 0
    }


toWorldInfo : Model -> WorldInfo
toWorldInfo model =
    let
        windAtPosition =
            \x y z ->
                model.windNoise model.time x y z
                    |> (*) (2 * pi)
                    |> (\turn -> ( cos turn, sin turn ))
    in
    { canvasHeight = model.height
    , canvasWidth = model.width
    , randomSeed = model.seed
    , windDirection = model.windDirection
    , gravity = Constants.gravity
    , windAtPosition = windAtPosition
    , debug = model.debug
    }


type Msg
    = Frame Float
    | BrowserResized Int Int
    | DebugChecked Bool
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
    in
    ( model
    , Random.generate GeneratedDrops
        (Random.list model.numberOfDrops (Raindrop.randomRainDrop (toWorldInfo model)))
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame deltaTime ->
            let
                ( newDrops, newSeed ) =
                    List.foldl
                        (\drop ( acc, oldSeed ) ->
                            drop
                                |> Raindrop.update deltaTime (toWorldInfo model) oldSeed
                                |> Tuple.mapFirst (List.singleton >> List.append acc)
                        )
                        ( [], model.seed )
                        (List.filter
                            (\drop ->
                                (List.length model.raindrops <= model.numberOfDrops)
                                    || not (Raindrop.isRaindropOffScreen model.height drop)
                            )
                            model.raindrops
                        )
            in
            ( { model
                | raindrops = newDrops
                , seed = newSeed
                , time = model.time + deltaTime
              }
            , Cmd.none
            )

        BrowserResized w h ->
            let
                width =
                    toFloat w

                height =
                    toFloat h
            in
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )

        DebugChecked value ->
            ( { model | debug = value }, Cmd.none )

        WindChanged direction ->
            ( { model | windDirection = direction }, Cmd.none )

        NumberOfDropsChanged numOfDrops ->
            let
                numberToGenerate =
                    max 0 numOfDrops - List.length model.raindrops
            in
            ( { model | numberOfDrops = numOfDrops }
            , Random.generate GeneratedDrops
                (Random.list numberToGenerate
                    (Raindrop.randomRainDrop (toWorldInfo model))
                )
            )

        GeneratedDrops newDrops ->
            ( { model | raindrops = newDrops ++ model.raindrops }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
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
                ( round model.width, round model.height )
                []
                [ clearScreen model.width model.height
                , Canvas.group []
                    (List.map (Raindrop.render (toWorldInfo model)) model.raindrops)
                ]
            ]
        , Html.div
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
                    , Attributes.checked model.debug
                    , Events.onCheck DebugChecked
                    ]
                    []
                , Html.text "Debug"
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
                    , Attributes.value <| String.fromFloat model.windDirection
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
                    , Attributes.value (String.fromInt model.numberOfDrops)
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
