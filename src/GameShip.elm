module GameShip exposing (..)


import GameCommon exposing (EmpireId, ShipId, Vector, vector, starSystemOuterRadius)
import Math.Vector2 as V



type ShipCommand
    = ThrustTo Vector


type alias Ship =
    { id : ShipId
    , empireId: EmpireId

    {- TODO
    ShipPosition =
        StarSystem starSystemId x y
        InFTL starSystemId StarSystemId completion
    -}
    , position : Vector
    , angle : Float

    , commands : List ShipCommand
    }




shipRateOfTurning =
    turns 0.02

shipThrust =
    starSystemOuterRadius / 100




idleBehavior ship =
    ship



thrustBehavior targetPosition ship =
    -- TODO: check that ship can actually move
    let
        difference =
            V.sub targetPosition ship.position

        distance =
            V.length difference

        targetAngle =
            if distance < starSystemOuterRadius / 1000
            then ship.angle
            else atan2 (V.getY difference) (V.getX difference)

        deltaAngle =
            clamp -shipRateOfTurning shipRateOfTurning (targetAngle - ship.angle)

        newAngle =
            ship.angle + deltaAngle

        speed =
            min distance shipThrust

        velocity =
            V.vec2 (speed * cos newAngle) (speed * sin newAngle)

        newPosition =
            V.add ship.position velocity
    in
        { ship
        | angle = newAngle
        , position = newPosition
        }



tick : Ship -> Ship
tick ship =
    case List.head ship.commands of
        Nothing -> idleBehavior ship
        Just command ->
            case command of
                ThrustTo position -> thrustBehavior position ship
