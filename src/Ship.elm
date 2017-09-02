module Ship exposing (..)

import Common exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)


-- This is used as base unit measure by the whole game


length =
    Common.shipLength


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
        (Vec2.fromTuple >> Vec2.scale (length / 15))
        [ ( -3, 0 )
        , ( -5, 5 )
        , ( 10, 0 )
        , ( -5, -5 )
        ]


convexMesh =
    List.drop 1 mesh



-- radius of the smallest circle (centered at the origin) that contains the mesh


radius =
    convexMesh
        |> List.map Vec2.length
        |> List.maximum
        |> Maybe.withDefault 0



-- HELPERS


transform : Ship -> List Vec2 -> List Vec2
transform ship polygon =
    List.map (rotateVector ship.heading >> Vec2.add ship.position) polygon
