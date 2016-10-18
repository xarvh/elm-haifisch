module Collision exposing (..)


import Array
import Common exposing (..)
import Math.Vector2 as V
import Ship


-- Collision detection



type alias Polygon =
    List Vector


anySegment : (( Vector, Vector ) -> Bool) -> Polygon -> Bool
anySegment f poly =
    let
        a =
            Array.fromList poly

        get index =
            Array.get (index % (Array.length a)) a |> Maybe.withDefault (vector 0 0)

        segments =
            List.indexedMap (\index v -> ( get index, get (index + 1) )) poly
    in
        List.any f segments


normalIsSeparatingAxis q ( a, b ) =
    let
        n =
            rightHandNormal <| V.sub b a

        isRightSide p =
            V.dot n (V.sub p a) > 0
    in
        List.all isRightSide q


halfCollision : Polygon -> Polygon -> Bool
halfCollision p q =
    -- https://www.toptal.com/game/video-game-physics-part-ii-collision-detection-for-solid-objects
    -- Try polygon p's normals as separating axies.
    -- If any of them does separe the polys, then the two polys are NOT intersecting
    not <| anySegment (normalIsSeparatingAxis q) p


collisionPolygonVsPolygon : Polygon -> Polygon -> Bool
collisionPolygonVsPolygon p q =
    halfCollision p q && halfCollision q p



-- a and b are the old and new positions of the projectile for this frame
projectileVsShip : Vector -> Vector -> Ship -> Bool
projectileVsShip a b ship =
    let
        -- get an easy-to-compute circle centered in A that contains B
        (dx, dy) =
            V.toTuple <| V.sub a b

        radius =
            max (abs dx) (abs dy)

        minimumCollisionDistance =
            radius + Ship.radius
    in
        if V.distanceSquared a ship.position > minimumCollisionDistance * minimumCollisionDistance then
            False
        else
            collisionPolygonVsPolygon [a, b] (Ship.transform ship Ship.convexMesh)

