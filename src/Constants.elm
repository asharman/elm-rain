module Constants exposing (WorldInfo, gravity, origin, windNoiseFromSeed)

import Random
import Simplex
import Vector exposing (Vector)


type alias WorldInfo =
    { canvasHeight : Float
    , canvasWidth : Float
    , randomSeed : Random.Seed
    , windDirection : Float
    , gravity : Vector
    , windAtPosition : Float -> Float -> Float -> Vector
    , debug : Bool
    }


origin : Vector
origin =
    ( 0, 0 )


gravity : Vector
gravity =
    ( 0, 1 )


windNoiseFromSeed : Int -> Float -> Float -> Float -> Float -> Float
windNoiseFromSeed =
    Simplex.fractal4d { scale = 30, steps = 1, stepSize = 0.1, persistence = 2.0 }
        << Simplex.permutationTableFromInt
