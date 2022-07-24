module Main exposing (main)

import Arrow
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Canvas.Settings.Line as Canvas
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style, width)
import Task



-- CONSTANTS


origin : Canvas.Point
origin =
    ( 0, 0 )


type alias Model =
    { width : Float, height : Float, position : Canvas.Point }


initialModel =
    { width = 400, height = 400, position = ( 200, 200 ) }


type Msg
    = Frame Float
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
                    Frame _ ->
                        ( model, Cmd.none )

                    GetViewPort data ->
                        ( { model
                            | width = data.viewport.width
                            , height = data.viewport.height
                            , position = ( data.viewport.width / 2, data.viewport.height / 2 )
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
                            , position = ( width / 2, height / 2 )
                          }
                        , Cmd.none
                        )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta Frame
                    , onResize BrowserResized
                    ]
        }


view : Model -> Html Msg
view { width, height, position } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round width, round height )
            []
            [ clearScreen width height
            , Arrow.render { from = origin, to = position, color = Color.rgb 0.3 0.3 1.0 }
            ]
        ]


clearScreen : Float -> Float -> Canvas.Renderable
clearScreen width height =
    Canvas.shapes [ Canvas.fill Color.darkCharcoal ] [ Canvas.rect ( 0, 0 ) width height ]
