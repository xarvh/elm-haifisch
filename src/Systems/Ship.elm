module Systems.Ship exposing (shipsSystems)

import Common
import Components exposing (EntityId)
import Dict exposing (Dict)
import Game exposing (Game)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)


turningRate =
    turns 1 / Time.second


shipHeading : Time -> Vec2 -> Float -> Game.InputState -> Float
shipHeading dt position currentHeading inputState =
    let
        targetDirection =
            case inputState.finalAim of
                Game.AimRelative direction ->
                    direction

                Game.AimAbsolute target ->
                    Vec2.sub target position

        targetHeading =
            Common.vectorToAngle targetDirection

        deltaHeading =
            Common.normalizeAngle <| targetHeading - currentHeading

        maxTurn =
            turningRate * dt

        clampedDeltaAngle =
            clamp -maxTurn maxTurn deltaHeading
    in
        Common.normalizeAngle <| currentHeading + clampedDeltaAngle


shipSystems : Float -> Game -> ( EntityId, Game.ShipComponent, Vec2, Float, EntityId ) -> ( EntityId, Game.ShipComponent, Float )
shipSystems dt game ( id, ship, position, heading, ownerId ) =
    let
        inputState =
            Components.get game.cPlayer ownerId
                |> Maybe.andThen .inputState
                |> Maybe.withDefault Game.neutralInputState

        newHeading =
            shipHeading dt position heading inputState
    in
        ( id, ship, newHeading )


shipsSystems : Time -> Game -> Game
shipsSystems dt game =
    let
        ships =
            ( .cShip, .cPosition, .cHeading, .cOwner )
                |> Components.all4 game
                |> List.map (shipSystems dt game)

        dHeading =
            ships
                |> List.map (\( id, ship, heading ) -> ( id, heading ))
                |> Dict.fromList
    in
        { game
            | cHeading = Dict.union dHeading game.cHeading
        }
