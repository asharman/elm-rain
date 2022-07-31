module Constants exposing (WorldInfo, gravity, origin)

import Random
import Vector exposing (Vector)


type alias WorldInfo =
    { canvasHeight : Float
    , canvasWidth : Float
    , randomSeed : Random.Seed
    }


origin : Vector
origin =
    ( 0, 0 )


gravity : Vector
gravity =
    ( 0, 0.75 )
