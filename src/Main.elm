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
import Raindrop exposing (Raindrop)
import Random
import Task



-- CONSTANTS


type alias Model =
    { width : Float
    , height : Float
    , raindrop : Raindrop
    , debug : Bool
    , numberOfDrops : Int
    , seed : Random.Seed
    }


initialModel : Model
initialModel =
    { width = 400
    , height = 400
    , raindrop = Raindrop.centeredRainDrop 400 400
    , debug = True
    , numberOfDrops = 1
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


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initialModel, Task.perform GetViewPort getViewport )
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
                ( newDrop, newSeed ) =
                    Raindrop.update deltaTime (toWorldInfo model) model.raindrop
            in
            ( { model
                | raindrop = newDrop
                , seed = newSeed
              }
            , Cmd.none
            )

        GetViewPort data ->
            ( { model
                | width = data.viewport.width
                , height = data.viewport.height
                , raindrop = Raindrop.centeredRainDrop data.viewport.width data.viewport.height
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
                , raindrop = Raindrop.centeredRainDrop width height
              }
            , Cmd.none
            )

        DebugChecked value ->
            ( { model | debug = value }, Cmd.none )

        NumberOfDropsChanged numOfDrops ->
            ( { model | numberOfDrops = numOfDrops }, Cmd.none )


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
                , Raindrop.render model.raindrop model.debug
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
