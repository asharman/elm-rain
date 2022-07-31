module Raindrop exposing (Raindrop, centeredRainDrop, position, randomRainDrop, render, update)

import Canvas
import Canvas.Settings as Canvas
import Color exposing (Color)
import Constants exposing (WorldInfo)
import Random
import Vector exposing (Vector)


type Raindrop
    = Internal
        { color : Color
        , position : Vector
        , velocity : Vector
        }


randomRainDrop : Float -> Random.Generator Raindrop
randomRainDrop width =
    Random.map
        (\pos ->
            Internal
                { color = Color.white
                , position = pos
                , velocity = Constants.gravity
                }
        )
        (Vector.randomVectorAboveCanvas width)


centeredRainDrop : Float -> Float -> Raindrop
centeredRainDrop width height =
    Internal
        { color = Color.white
        , position = ( width / 2, height / 2 )
        , velocity = Constants.gravity
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
            Canvas.text [] Constants.origin ""
        , Canvas.shapes
            [ Canvas.fill drop.color
            ]
            [ Canvas.rect drop.position 4 20 ]
        ]


update : Float -> WorldInfo -> Raindrop -> Raindrop
update deltaTime worldInfo (Internal drop) =
    let
        scaledVelocity =
            Vector.scale deltaTime Constants.gravity

        ( updatedPosition, _ ) =
            if isRaindropOffScreen worldInfo.canvasHeight drop.position then
                Random.step (Vector.randomVectorAboveCanvas worldInfo.canvasWidth) worldInfo.randomSeed

            else
                ( Vector.add drop.position scaledVelocity, worldInfo.randomSeed )
    in
    Internal
        { drop
            | velocity = scaledVelocity
            , position = updatedPosition
        }


isRaindropOffScreen : Float -> Vector -> Bool
isRaindropOffScreen screenHeight ( _, yPos ) =
    yPos > screenHeight


renderDebug : Raindrop -> Canvas.Renderable
renderDebug (Internal drop) =
    Canvas.group []
        [ Vector.renderArrow
            { from = Constants.origin
            , to = drop.position
            , color = Color.rgb 0.25 0.25 1.0
            }
        , Vector.renderArrow
            { from = drop.position
            , to = Vector.add drop.position drop.velocity
            , color = Color.rgb 1.0 0.25 0.25
            }
        ]
