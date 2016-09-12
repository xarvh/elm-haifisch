module GameFleet exposing (..)


import Math.Vector2 as V

import GameCommon as G exposing
    ( Game
    , GameEffect
    , Fleet
    , Ship
    , EmpireId
    , FleetId
    , Vector
    , vector
    , starSystemOuterRadius
    )



-- CONSTANTS


shipRateOfTurning =
    turns 0.05


shipThrust =
    starSystemOuterRadius / 100


mergeDistance =
    0.005 * starSystemOuterRadius



-- HELPERS


popCommand fleet =
    { fleet | commands = List.drop 1 fleet.commands }


noEffect fleet =
    ( fleet, [] )








-- INIT


init id empireId position =
    let
        formationDirection =
            V.normalize <| V.negate position

        ship =
            Ship position position 0 False
    in
        Fleet id 0 (List.repeat 2 ship) formationDirection position []




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




rotate90 v =
    let
        (x, y) = V.toTuple v
    in
        vector (y) (-x)



fleetPosition fleet =
    List.head fleet.ships
    |> Maybe.map .currentPosition
    |> Maybe.withDefault (vector 0 0)





formation : Vector -> Fleet -> List Ship
formation targetPosition fleet =
    let
        formationOrthogonal =
            rotate90 fleet.formationDirection

        formationScale =
            0.08

        folder ship (ships, row, column) =
            let

                r =
                    V.scale (-row * formationScale) fleet.formationDirection

                c =
                    V.scale (column * formationScale) formationOrthogonal

                pos =
                    V.add targetPosition <| V.add r c

                newShip =
                    { ship | targetPosition = pos }

                (nextRow, nextColumn) =
                    if column == row
                    then (row + 1, 0)
                    else (row, column + 1)
            in
                (List.append ships [newShip], nextRow, nextColumn)


        (newShips, row, column) =
            List.foldl folder ([], 0, 0) fleet.ships
    in
        newShips



thrustToBehavior targetPosition oldFleet =
    let
        -- TODO: check that fleet can actually move (for ex, it is NOT in FTL)

        -- if target changed, update formation direction
        (newFormationDirection, newFormationTarget) =
            if targetPosition == oldFleet.formationTarget
            then ( oldFleet.formationDirection, oldFleet.formationTarget )
            else
                -- if updating formationDirection would result into a NaN, maintain the old one
                if targetPosition == fleetPosition oldFleet
                then ( oldFleet.formationDirection, targetPosition )
                else ( V.direction targetPosition (fleetPosition oldFleet), targetPosition )

        newFleet =
            { oldFleet
            | formationDirection = newFormationDirection
            , formationTarget = newFormationTarget
            }

        newShips =
            formation targetPosition newFleet
            |> List.map shipThrusts

        isStillMoving =
            List.any .isThrusting newShips

        updateCommands =
            if isStillMoving
            then identity
            else popCommand
    in
        updateCommands { newFleet | ships = newShips }








mergeWithBehavior : Game -> Int -> Fleet -> ( Fleet, List GameEffect )
mergeWithBehavior game targetFleetId oldFleet =
    case G.findId targetFleetId game.fleets of

        Nothing ->
            noEffect <| popCommand oldFleet

        Just targetFleet ->
            let
                pos = fleetPosition oldFleet
                targ = fleetPosition targetFleet

                ( effects, updateCommands )=
                    if V.distanceSquared pos targ > mergeDistance * mergeDistance
                    then ( [], thrustToBehavior targ )
                    else ( [G.MergeFleets oldFleet.id targetFleetId], popCommand )
            in
                ( updateCommands oldFleet, effects )




tick : Game -> Fleet -> ( Fleet, List GameEffect )
tick game fleet =
    case List.head fleet.commands of

        Nothing ->
            noEffect <| idleBehavior fleet

        Just command ->
            case command of

                G.ThrustTo targetPosition ->
                    noEffect <| thrustToBehavior targetPosition fleet

                G.MergeWith targetFleetId ->
                    mergeWithBehavior game targetFleetId fleet
