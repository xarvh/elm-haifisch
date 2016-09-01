module GameFleet exposing (..)


import GameCommon as G exposing (EmpireId, FleetId, Vector, vector, starSystemOuterRadius)
import Math.Vector2 as V




type alias Ship =
    { currentPosition : Vector
    , targetPosition : Vector
    , angle : Float
    , isThrusting : Bool
    }



type alias Fleet =
    { id : FleetId
    , empireId : EmpireId

    , ships : List Ship

    {- TODO
    Position =
        StarSystem starSystemId
        InFTL starSystemId StarSystemId completion
    -}

    , commands : List G.FleetCommand
    }



shipRateOfTurning =
    turns 0.02


shipThrust =
    starSystemOuterRadius / 100


idleBehavior fleet =
    fleet


normalizeAngle a =
    if a < -pi
    then a + 2 * pi
    else if a > pi
        then a - 2 * pi
        else a



shipThrusts : Ship -> Ship
shipThrusts ship =
    let
        difference =
            V.sub ship.targetPosition ship.currentPosition

        distance =
            V.length difference

        distanceIsInsignificant =
            distance < starSystemOuterRadius / 1000

    in
        if distanceIsInsignificant
        then { ship | isThrusting = False }
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
                    V.add ship.currentPosition velocity
            in
                { ship
                | angle = newAngle
                , currentPosition = newPosition
                , isThrusting = True
                }



formation : Vector -> List Ship -> List Ship
formation targetPosition ships =
    let
        initialPosition =
            List.head ships
            |> Maybe.map .currentPosition
            |> Maybe.withDefault (vector 0 0)

        formationDirection =
            V.direction targetPosition initialPosition

        folder ship (ships, row, column) =
            let
                -- TODO: rotate to match formationDirection
                pos =
                    V.add targetPosition <| vector (row / 0.01) (column / 0.01)

                newShip =
                    { ship | targetPosition = pos }

                (nextRow, nextColumn) =
                    if column == row
                    then (row + 1, 0)
                    else (row, column + 1)
            in
                (List.append ships [newShip], row, column)


        (newShips, row, column) =
            List.foldl folder ([], 0, 0) ships
    in
        newShips



thrustToBehavior targetPosition fleet =
    let
        -- TODO: check that fleet can actually move (for ex, it is NOT in FTL)

        newShips =
            formation targetPosition fleet.ships
            |> List.map shipThrusts

        isStillMoving =
            List.any .isThrusting newShips

        newCommands =
            if isStillMoving
            then fleet.commands
            else List.drop 1 fleet.commands
    in
        { fleet
        | ships = newShips
        , commands = newCommands
        }



tick : Fleet -> Fleet
tick fleet =
    case List.head fleet.commands of

        Nothing ->
            idleBehavior fleet

        Just command ->
            case command of
                G.ThrustTo targetPosition ->
                    thrustToBehavior targetPosition fleet
