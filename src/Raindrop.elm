module Raindrop exposing (Raindrop, centeredRainDrop, position, randomRainDrop, render, update)

import Canvas
import Canvas.Settings as Canvas
import Color exposing (Color)
import Random
import Vector exposing (Vector, randomVectorInCanvas)


type Raindrop
    = Internal
        { color : Color
        , position : Vector
        , velocity : Vector
        }


randomRainDrop : Float -> Float -> Random.Generator Raindrop
randomRainDrop width height =
    Random.map
        (\pos ->
            Internal
                { color = Color.white
                , position = pos
                , velocity = Vector.gravity
                }
        )
        (randomVectorInCanvas width height)


centeredRainDrop : Float -> Float -> Raindrop
centeredRainDrop width height =
    Internal
        { color = Color.white
        , position = ( width / 2, height / 2 )
        , velocity = Vector.gravity
        }


position : Raindrop -> Vector
position (Internal data) =
    data.position


render : Raindrop -> Bool -> Canvas.Renderable
render (Internal drop) debug =
    Canvas.group []
        [ if debug then
            renderDebug (Internal drop)

          else
            Canvas.text [] Vector.origin ""
        , Canvas.shapes
            [ Canvas.fill drop.color
            ]
            [ Canvas.rect drop.position 4 20 ]
        ]


update : Float -> Raindrop -> Raindrop
update deltaTime (Internal drop) =
    let
        scaledVelocity =
            Vector.scale deltaTime Vector.gravity
    in
    Internal
        { drop
            | velocity = scaledVelocity
            , position = Vector.add drop.position scaledVelocity
        }


renderDebug : Raindrop -> Canvas.Renderable
renderDebug (Internal drop) =
    Canvas.group []
        [ Vector.renderArrow
            { from = Vector.origin
            , to = drop.position
            , color = Color.rgb 0.25 0.25 1.0
            }
        , Vector.renderArrow
            { from = drop.position
            , to = Vector.add drop.position drop.velocity
            , color = Color.rgb 1.0 0.25 0.25
            }
        ]
