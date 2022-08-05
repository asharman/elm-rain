module Raindrop exposing (Raindrop, isRaindropOffScreen, randomRainDrop, render, update)

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


render : Bool -> Raindrop -> Canvas.Renderable
render debug (Internal drop) =
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


update : Float -> WorldInfo -> Random.Seed -> Raindrop -> ( Raindrop, Random.Seed )
update deltaTime worldInfo newSeed (Internal drop) =
    let
        scaledVelocity =
            Vector.scale deltaTime Constants.gravity

        ( updatedPosition, seed ) =
            if isRaindropOffScreen worldInfo.canvasHeight (Internal drop) then
                Random.step (Vector.randomVectorAboveCanvas worldInfo.canvasWidth) newSeed

            else
                ( Vector.add drop.position scaledVelocity, newSeed )
    in
    ( Internal
        { drop
            | velocity = scaledVelocity
            , position = updatedPosition
        }
    , seed
    )


isRaindropOffScreen : Float -> Raindrop -> Bool
isRaindropOffScreen screenHeight (Internal drop) =
    Tuple.second drop.position > screenHeight


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
