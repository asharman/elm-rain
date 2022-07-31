module Vector exposing (Vector, origin, randomVectorInCanvas, renderArrow)

import Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Line as Canvas
import Color
import Random


origin : Vector
origin =
    ( 0, 0 )


type alias Vector =
    ( Float, Float )


randomVectorInCanvas : Float -> Float -> Random.Generator Vector
randomVectorInCanvas width height =
    let
        randomFloat limit =
            Random.float 0.0 limit
    in
    Random.map2 (\w h -> ( w, h )) (randomFloat width) (randomFloat height)


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
