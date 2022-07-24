module Main exposing (main)

import Arrow
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Canvas.Settings.Line as Canvas
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)



-- CONSTANTS


width =
    1000


height =
    800


centerX =
    width / 2


centerY =
    height / 2


origin : Canvas.Point
origin =
    ( 0, 0 )


type alias Model =
    { position : Canvas.Point }


initialModel =
    { position = ( centerX, centerY ) }


type Msg
    = Frame Float


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initialModel, Cmd.none )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Frame _ ->
                        ( model, Cmd.none )
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }


view : Model -> Html Msg
view { position } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            []
            [ clearScreen
            , Arrow.render { from = origin, to = position, color = Color.rgb 0.3 0.3 1.0 }
            ]
        ]


clearScreen : Canvas.Renderable
clearScreen =
    Canvas.shapes [ Canvas.fill Color.darkCharcoal ] [ Canvas.rect ( 0, 0 ) width height ]
