module Vector exposing
    ( Vector
    , add
    , limitMagnitude
    , magnitude
    , normalize
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


magnitude : Vector -> Float
magnitude ( x, y ) =
    sqrt (x * x + y * y)


normalize : Vector -> Vector
normalize ( x, y ) =
    let
        mag =
            magnitude ( x, y )
    in
    ( x / mag, y / mag )


limitMagnitude : Float -> Vector -> Vector
limitMagnitude mag vec =
    if abs (magnitude vec) > mag then
        vec |> normalize |> scale mag

    else
        vec


randomVectorAboveCanvas : Float -> Random.Generator Vector
randomVectorAboveCanvas width =
    Random.map2 (\w h -> ( w, h )) (Random.float -500.0 (width + 500)) (Random.float -500.0 -50.0)


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
