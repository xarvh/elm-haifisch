module GameShip exposing (..)


import GameCommon as G exposing (EmpireId, ShipId, Vector, vector, starSystemOuterRadius)
import Math.Vector2 as V



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

    , commands : List G.ShipCommand
    }




shipRateOfTurning =
    turns 0.02


shipThrust =
    starSystemOuterRadius / 100


idleBehavior ship =
    ship


normalizeAngle a =
    if a < -pi
    then a + 2 * pi
    else if a > pi
        then a - 2 * pi
        else a


thrustToBehavior targetPosition ship =
    -- TODO: check that ship can actually move (for ex, it is NOT in FTL)
    let
        difference =
            V.sub targetPosition ship.position

        distance =
            V.length difference

        distanceIsInsignificant =
            distance < starSystemOuterRadius / 1000
    in
        if distanceIsInsignificant
        then { ship | commands = List.drop 1 ship.commands }
        else
            let
                targetAngle =
                    if distanceIsInsignificant
                    then ship.angle
                    else atan2 (V.getY difference) (V.getX difference)

                deltaAngle =
                    (targetAngle - ship.angle)

                clampedDeltaAngle =
                    clamp -shipRateOfTurning shipRateOfTurning (normalizeAngle deltaAngle)

                newAngle =
                    ship.angle + clampedDeltaAngle

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

        Nothing ->
            idleBehavior ship

        Just command ->
            case command of
                G.ThrustTo position ->
                    thrustToBehavior position ship
