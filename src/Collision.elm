module Collision exposing (..)

import Array
import Common exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Ship


-- Collision detection


type alias Polygon =
    List Vec2


anySegment : (( Vec2, Vec2 ) -> Bool) -> Polygon -> Bool
anySegment f poly =
    let
        a =
            Array.fromList poly

        get index =
            Array.get (index % (Array.length a)) a |> Maybe.withDefault (vec2 0 0)

        segments =
            List.indexedMap (\index v -> ( get index, get (index + 1) )) poly
    in
        List.any f segments


normalIsSeparatingAxis q ( a, b ) =
    let
        n =
            rightHandNormal <| Vec2.sub b a

        isRightSide p =
            Vec2.dot n (Vec2.sub p a) > 0
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


projectileVsShip : Vec2 -> Vec2 -> Ship -> Bool
projectileVsShip a b ship =
    let
        -- get an easy-to-compute circle centered in A that contains B
        ( dx, dy ) =
            Vec2.toTuple <| Vec2.sub a b

        radius =
            max (abs dx) (abs dy)

        minimumCollisionDistance =
            radius + Ship.radius
    in
        if Vec2.distanceSquared a ship.position > minimumCollisionDistance * minimumCollisionDistance then
            False
        else
            collisionPolygonVsPolygon [ a, b ] (Ship.transform ship Ship.convexMesh)
