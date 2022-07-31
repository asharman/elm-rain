module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Canvas.Settings as Canvas
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Raindrop exposing (Raindrop)
import Task
import Vector exposing (Vector)



-- CONSTANTS


origin : Vector
origin =
    ( 0, 0 )


type alias Model =
    { width : Float, height : Float, raindrop : Raindrop }


initialModel : Model
initialModel =
    { width = 400, height = 400, raindrop = Raindrop.centeredRainDrop 400 400 }


type Msg
    = Frame
    | GetViewPort Viewport
    | BrowserResized Int Int


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initialModel, Task.perform GetViewPort getViewport )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Frame ->
                        ( model, Cmd.none )

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
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta (\_ -> Frame)
                    , onResize BrowserResized
                    ]
        }


view : Model -> Html Msg
view { width, height, raindrop } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round width, round height )
            []
            [ clearScreen width height
            , Raindrop.render raindrop
            , Vector.renderArrow
                { from = origin
                , to = Raindrop.position raindrop
                , color = Color.rgb 0.25 0.25 1.0
                }
            ]
        ]


clearScreen : Float -> Float -> Canvas.Renderable
clearScreen width height =
    Canvas.shapes [ Canvas.fill Color.black ]
        [ Canvas.rect ( 0, 0 ) width height ]
