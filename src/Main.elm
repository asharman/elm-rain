module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
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
import Task



-- CONSTANTS


type alias Model =
    { width : Float
    , height : Float
    , raindrops : List Raindrop
    , debug : Bool
    , numberOfDrops : Int
    , seed : Random.Seed
    }


initialModel : Model
initialModel =
    { width = 400
    , height = 400
    , raindrops = []
    , debug = True
    , numberOfDrops = 10
    , seed = Random.initialSeed 1
    }


toWorldInfo : Model -> WorldInfo
toWorldInfo model =
    { canvasHeight = model.height
    , canvasWidth = model.width
    , randomSeed = model.seed
    }


type Msg
    = Frame Float
    | GetViewPort Viewport
    | BrowserResized Int Int
    | DebugChecked Bool
    | NumberOfDropsChanged Int
    | GeneratedDrops (List Raindrop)


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                ( initialModel
                , Cmd.batch
                    [ Task.perform GetViewPort getViewport
                    , Random.generate GeneratedDrops
                        (Random.list initialModel.numberOfDrops (Raindrop.randomRainDrop initialModel.width))
                    ]
                )
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta Frame
                    , onResize BrowserResized
                    ]
        }


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
              }
            , Cmd.none
            )

        GetViewPort data ->
            ( { model
                | width = data.viewport.width
                , height = data.viewport.height
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

        NumberOfDropsChanged numOfDrops ->
            let
                numberToGenerate =
                    max 0 numOfDrops - List.length model.raindrops
            in
            ( { model | numberOfDrops = numOfDrops }
            , Random.generate GeneratedDrops (Random.list numberToGenerate (Raindrop.randomRainDrop model.width))
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
                    (List.map (Raindrop.render model.debug) model.raindrops)
                ]
            ]
        , Html.div
            [ Attributes.style "position" "absolute"
            , Attributes.style "bottom" "1rem"
            , Attributes.style "left" "1rem"
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
                []
                [ Html.input
                    [ Attributes.type_ "number"
                    , Attributes.max "100"
                    , Attributes.min "1"
                    , Attributes.value (String.fromInt model.numberOfDrops)
                    , onPositiveIntInput NumberOfDropsChanged
                    ]
                    []
                ]
            ]
        ]


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
