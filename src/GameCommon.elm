module GameCommon exposing (..)


import Math.Vector2


-- IDs


type alias Id = Int
type alias EmpireId = Id
type alias ShipId = Id



-- STAR SYSTEM COORDINATES


type alias Vector =
    Math.Vector2.Vec2

vector =
    Math.Vector2.vec2

vectorToString : Vector -> String
vectorToString v =
    "(" ++ toString (Math.Vector2.getX v) ++ "," ++ toString (Math.Vector2.getY v) ++ ")"


-- COMMANDS


type QueueMode
    = AddToExistingCommandQueue
    | CancelExistingCommandQueue


type Command
    = ShipMove (List ShipId) Vector


-- This is the max distance a ship can be from a star outside FTL
starSystemOuterRadius : Float
starSystemOuterRadius =
    0.96

