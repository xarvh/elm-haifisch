module GameCommon exposing (..)


import Math.Vector2 as V


-- IDs


type alias Id = Int
type alias EmpireId = Id
type alias ShipId = Id



-- STAR SYSTEM COORDINATES


type alias Vector =
    V.Vec2

vector =
    V.vec2

vectorToString : Vector -> String
vectorToString v =
    toString (V.getX v) ++ "," ++ toString (V.getY v)


-- COMMANDS


type QueueMode
    = Append
    | Replace



type ShipCommand
    = ThrustTo Vector



type Command
    = ShipCommand (List ShipId) QueueMode ShipCommand


-- This is the max distance a ship can be from a star outside FTL
starSystemOuterRadius : Float
starSystemOuterRadius =
    0.96


-- GEOMETRY

normalizeBox a b =
    let
        ax = V.getX a
        ay = V.getY a
        bx = V.getX b
        by = V.getY b
    in
        ( min ax bx
        , min ay by
        , max ax bx
        , max ay by
        )

