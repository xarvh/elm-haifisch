module GameCommon exposing (..)


import Math.Vector2


type alias Id = Int
type alias EmpireId = Id
type alias ShipId = Id



type alias Pos =
    Math.Vector2.Vec2

pos =
    Math.Vector2.vec2



type QueueMode
    = AddToExistingCommandQueue
    | CancelExistingCommandQueue





type TurnDirection
    = Clockwise
    | CounterClockwise



type alias Vector =
    { x : Float
    , y : Float
    }




-- This is the max distance a ship can be from a star outside FTL
starSystemOuterRadius : Float
starSystemOuterRadius =
    0.96




