module Raindrop exposing (Raindrop, isRaindropOffScreen, randomRainDrop, render, update)

import Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Color exposing (Color)
import Constants exposing (WorldInfo)
import Random
import Vector exposing (Vector)


type Raindrop
    = Internal
        { color : Color
        , position : Vector
        , velocity : Vector
        , distanceFromScreen : Float
        }


randomRainDrop : WorldInfo -> Random.Generator Raindrop
randomRainDrop worldInfo =
    Random.map2
        (\pos distance ->
            Internal
                { color = Color.white
                , position = pos
                , velocity = Constants.gravity
                , distanceFromScreen = distance
                }
        )
        (Vector.randomVectorAboveCanvas worldInfo.canvasWidth)
        (Random.float 0.4 1.0)


render : WorldInfo -> Raindrop -> Canvas.Renderable
render worldInfo (Internal drop) =
    let
        xPos =
            Tuple.first drop.position

        yPos =
            Tuple.second drop.position

        xVel =
            Tuple.first drop.velocity

        yVel =
            Tuple.second drop.velocity
    in
    Canvas.group []
        [ if worldInfo.debug then
            renderDebug worldInfo (Internal drop)

          else
            Canvas.text [] Constants.origin ""
        , Canvas.shapes
            [ Canvas.fill drop.color
            , Canvas.transform
                [ Canvas.translate xPos (yPos + 15 * drop.distanceFromScreen)
                , Canvas.rotate <| atan2 -xVel yVel
                , Canvas.translate -xPos (-yPos + 15 * drop.distanceFromScreen)
                ]
            ]
            [ Canvas.rect drop.position (4 * drop.distanceFromScreen) (30 * drop.distanceFromScreen) ]
        ]


update : Float -> WorldInfo -> Random.Seed -> Raindrop -> ( Raindrop, Random.Seed )
update deltaTime worldInfo newSeed (Internal drop) =
    let
        ( xPos, yPos ) =
            drop.position

        acceleration =
            List.foldl Vector.add Constants.origin <|
                [ worldInfo.gravity
                , ( worldInfo.windDirection * 0.5, 0 )
                , Vector.scale 0.25 (worldInfo.windAtPosition xPos yPos drop.distanceFromScreen)
                ]

        velocity =
            Vector.limitMagnitude 5 (Vector.add drop.velocity acceleration)

        scaledVelocity =
            Vector.scale (deltaTime * 0.5 * drop.distanceFromScreen) velocity
    in
    if isRaindropOffScreen worldInfo.canvasHeight (Internal drop) then
        Random.step (randomRainDrop worldInfo) newSeed

    else
        ( Internal
            { drop
                | velocity = scaledVelocity
                , position = Vector.add drop.position scaledVelocity
            }
        , newSeed
        )


isRaindropOffScreen : Float -> Raindrop -> Bool
isRaindropOffScreen screenHeight (Internal drop) =
    Tuple.second drop.position > screenHeight


renderDebug : WorldInfo -> Raindrop -> Canvas.Renderable
renderDebug worldInfo (Internal drop) =
    Canvas.group []
        [ Vector.renderArrow
            { from = Constants.origin
            , to = drop.position
            , color = Color.rgb 0.25 0.25 1.0
            }
        , Vector.renderArrow
            { from = drop.position
            , to = Vector.add drop.position (Vector.scale 100 worldInfo.gravity)
            , color = Color.rgb 1.0 0.25 0.25
            }
        , if worldInfo.windDirection == 0 then
            Canvas.text [] Constants.origin ""

          else
            Vector.renderArrow
                { from = drop.position
                , to = Vector.add drop.position (Vector.scale 150 ( worldInfo.windDirection, 0 ))
                , color = Color.rgb 0.25 1.0 0.25
                }
        , Vector.renderArrow
            { from = drop.position
            , to = Vector.add drop.position (Vector.scale 10 drop.velocity)
            , color = Color.rgb 1.0 1.0 1.0
            }
        ]
