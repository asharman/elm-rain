module Raindrop exposing (Raindrop, centeredRainDrop, position, randomRainDrop, render)

import Canvas
import Canvas.Settings as Canvas
import Color exposing (Color)
import Random
import Vector exposing (Vector, randomVectorInCanvas)


type Raindrop
    = Internal
        { color : Color
        , position : Vector
        }


randomRainDrop : Float -> Float -> Random.Generator Raindrop
randomRainDrop width height =
    Random.map (\pos -> Internal { color = Color.white, position = pos })
        (randomVectorInCanvas width height)


centeredRainDrop : Float -> Float -> Raindrop
centeredRainDrop width height =
    Internal
        { color = Color.white
        , position = ( width / 2, height / 2 )
        }


position : Raindrop -> Vector
position (Internal data) =
    data.position


render : Raindrop -> Bool -> Canvas.Renderable
render (Internal drop) debug =
    Canvas.group []
        [ Canvas.shapes
            [ Canvas.fill drop.color
            ]
            [ Canvas.rect drop.position 4 20 ]
        , if debug then
            renderDebug (Internal drop)

          else
            Canvas.text [] Vector.origin ""
        ]


renderDebug : Raindrop -> Canvas.Renderable
renderDebug (Internal drop) =
    Vector.renderArrow
        { from = Vector.origin
        , to = drop.position
        , color = Color.rgb 0.25 0.25 1.0
        }
