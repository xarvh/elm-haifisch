module GameShip exposing (..)


import GameCommon exposing (EmpireId, ShipId, Vector, vector)



type ShipCommand
    = Goto Int Int


type alias Ship =
    { id : ShipId
    , empireId: EmpireId

    {- TODO
    ShipPosition =
        StarSystem starSystemId x y
        InFTL starSystemId StarSystemId completion
    -}
    , position : Vector
    , velocity : Vector
    , angle : Float

    , commands : List ShipCommand
    }






-- init =
--     Ship { x = size/2, y = size/2} { x = 0, y = 0 } 0 False Nothing



-- wrap size value =
--    value - (toFloat <| floor <| value / size) * size
-- 
-- 
-- updatePositionByComponent deltaTime model component =
--     wrap size <| component model.position + component model.velocity * deltaTime
-- 
-- 
-- updateVelocityByComponent deltaTime thrust model component =
--     clamp (-maxVelocity) maxVelocity <| component model.velocity + component thrust * deltaTime



tick : Ship -> Ship
tick ship =
    ship
--     let
--         pos =
--             updatePositionByComponent dt model
-- 
--         newPosition =
--             { x = pos .x, y = pos .y }
-- 
--         thrust =
--             if model.commandThrust
--             then { x = shipThrust * cos model.angle, y = shipThrust * sin model.angle }
--             else { x = 0, y = 0 }
-- 
--         vel =
--             updateVelocityByComponent dt thrust model
-- 
--         newVelocity =
--             { x = vel .x, y = vel .y }
-- 
--         newAngle =
--             case model.commandTurn of
--                 Nothing -> model.angle
--                 Just direction ->
--                     wrap (turns 1) <| model.angle + shipTurnRate * dt * case direction of
--                         Clockwise -> -1
--                         CounterClockwise -> 1
--     in
--         { model
--         | position = newPosition
--         , velocity = newVelocity
--         , angle = newAngle
--         }

