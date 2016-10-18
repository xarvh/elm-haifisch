module Ship exposing (..)

import Math.Vector2 as V
import Time exposing (Time)


-- This is used as base unit measure by the whole game
length =
    1


speed =
    5 * length / Time.second


turningRate =
    turns 1 / Time.second


reloadTime =
    0.3 * Time.second


explosionDuration =
    1000 * Time.millisecond


spawnDuration =
    3 * Time.second






{-
   Ship vertexes, clockwise from the rear
    ___
    \  --__
    /__--

-}


mesh =
    List.map
        (V.fromTuple >> V.scale (length / 15))
        [ ( -3, 0 )
        , ( -5, 5 )
        , ( 10, 0 )
        , ( -5, -5 )
        ]


convexMesh =
    List.drop 1 mesh



-- radius of the smallest circle (centered at the origin) that contains the mesh


radius =
    List.maximum <| List.map V.length convexMesh
