module GameCommon exposing (..)




type alias Id = Int
type alias EmpireId = Id
type alias ShipId = Id




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




