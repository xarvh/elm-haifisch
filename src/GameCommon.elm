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

