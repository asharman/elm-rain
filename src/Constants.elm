module Constants exposing (WorldInfo, gravity, origin)

import Random
import Vector exposing (Vector)


type alias WorldInfo =
    { canvasHeight : Float
    , canvasWidth : Float
    , randomSeed : Random.Seed
    , windDirection : Float
    , gravity : Vector
    , debug : Bool
    }


origin : Vector
origin =
    ( 0, 0 )


gravity : Vector
gravity =
    ( 0, 1 )
