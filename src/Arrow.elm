module Arrow exposing (render)

import Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Line as Canvas
import Color


type alias Config =
    { from : Canvas.Point
    , to : Canvas.Point
    , color : Color.Color
    }


render : Config -> Canvas.Renderable
render { from, to, color } =
    let
        fromX =
            Tuple.first from

        fromY =
            Tuple.second from

        toX =
            Tuple.first to

        toY =
            Tuple.second to

        slopeAngle =
            atan2 (toY - fromY) (toX - fromX)
    in
    Canvas.shapes
        [ Canvas.stroke color
        , Canvas.lineWidth 5.0
        , Canvas.lineCap Canvas.RoundCap
        ]
        [ Canvas.path from <|
            [ Canvas.lineTo to
            , Canvas.moveTo to
            , Canvas.lineTo ( toX - (15 * cos (slopeAngle + (pi / 7))), toY - (15 * sin (slopeAngle + pi / 7)) )
            , Canvas.lineTo ( toX - (15 * cos (slopeAngle - (pi / 7))), toY - (15 * sin (slopeAngle - pi / 7)) )
            , Canvas.lineTo to
            ]
        ]
