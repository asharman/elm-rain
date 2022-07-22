module Main exposing (main)

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
            , renderArrow position
            ]
        ]


clearScreen : Canvas.Renderable
clearScreen =
    Canvas.shapes [ Canvas.fill Color.darkCharcoal ] [ Canvas.rect ( 0, 0 ) width height ]


renderArrow : Canvas.Point -> Canvas.Renderable
renderArrow position =
    let
        x =
            Tuple.first position

        y =
            Tuple.second position

        slopeAngle =
            atan2 y x
    in
    Canvas.shapes
        [ Canvas.stroke (Color.rgb 0.2 0.2 0.7)
        , Canvas.fill (Color.rgb 0.2 0.2 0.7)
        , Canvas.lineWidth 5.0
        , Canvas.lineCap Canvas.RoundCap
        ]
        [ Canvas.path ( 0, 0 ) <|
            [ Canvas.lineTo position
            , Canvas.moveTo position
            , Canvas.lineTo ( x - (15 * cos (slopeAngle + (pi / 7))), y - (15 * sin (slopeAngle + pi / 7)) )
            , Canvas.lineTo ( x - (15 * cos (slopeAngle - (pi / 7))), y - (15 * sin (slopeAngle - pi / 7)) )
            , Canvas.lineTo position
            ]
        ]
