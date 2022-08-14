module Constants exposing (WorldInfo, gravity, origin, randomColor, windNoiseFromSeed)

import Color exposing (Color)
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
    Simplex.fractal4d { scale = 100, steps = 1, stepSize = 1, persistence = 1 }
        << Simplex.permutationTableFromInt


randomColor : Random.Generator Color
randomColor =
    Random.float 0 1
        |> Random.map
            (\hue -> Color.hsl hue 1 0.75)
