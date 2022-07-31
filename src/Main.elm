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
import Raindrop exposing (Raindrop)
import Random
import Task



-- CONSTANTS


type alias Model =
    { width : Float
    , height : Float
    , raindrop : Raindrop
    , debug : Bool
    , seed : Random.Seed
    }


initialModel : Model
initialModel =
    { width = 400
    , height = 400
    , raindrop = Raindrop.centeredRainDrop 400 400
    , debug = True
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
            ( { model
                | raindrop = Raindrop.update deltaTime (toWorldInfo model) model.raindrop
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


view : Model -> Html Msg
view { width, height, raindrop, debug } =
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
                ( round width, round height )
                []
                [ clearScreen width height
                , Raindrop.render raindrop debug
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
                    , Attributes.checked debug
                    , Events.onCheck DebugChecked
                    ]
                    []
                , Html.text "Debug"
                ]
            ]
        ]


clearScreen : Float -> Float -> Canvas.Renderable
clearScreen width height =
    Canvas.shapes [ Canvas.fill Color.black ]
        [ Canvas.rect ( 0, 0 ) width height ]
