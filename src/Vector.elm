module Vector exposing
    ( Vector
    , add
    , randomVectorAboveCanvas
    , renderArrow
    , scale
    )

import Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Line as Canvas
import Color
import Random


type alias Vector =
    ( Float, Float )


add : Vector -> Vector -> Vector
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


scale : Float -> Vector -> Vector
scale factor ( x, y ) =
    ( x * factor, y * factor )


randomVectorAboveCanvas : Float -> Random.Generator Vector
randomVectorAboveCanvas width =
    Random.map2 (\w h -> ( w, h )) (Random.float 0.0 width) (Random.float -100.0 -10.0)


renderArrow :
    { from : Vector
    , to : Vector
    , color : Color.Color
    }
    -> Canvas.Renderable
renderArrow { from, to, color } =
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
