module Systems.Movement exposing (..)

import Common
import Components exposing (EntityId)
import Dict exposing (Dict)
import Game exposing (Game)
import Time exposing (Time)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


speed =
    7 * Common.shipLength / Time.second


{-| TODO: this should probably live in a "ship control" system?
-}
shipVelocity : Game -> ( EntityId, Game.ShipComponent ) -> Vec2
shipVelocity game ( id, ship ) =
    case ship.status of
        Game.ShipExploding ->
            vec2 0 0

        _ ->
            -- TODO: exploding ships can't move
            Game.shipOwner game id
                |> Maybe.andThen .inputState
                |> Maybe.map (.move >> Vec2.scale speed)
                |> Maybe.withDefault (vec2 0 0)


entityVelocity : Game -> EntityId -> Vec2
entityVelocity game id =
    case Components.get game.cShip id of
        Just ship ->
            shipVelocity game ( id, ship )

        Nothing ->
            -- TODO projectiles and planets
            vec2 0 0


moveEntity : Game -> Time -> ( EntityId, Vec2 ) -> ( EntityId, Vec2 )
moveEntity game dt ( id, x ) =
    let
        velocity =
            entityVelocity game id

        dx =
            Vec2.scale dt velocity

        newPosition =
            Vec2.add x dx
    in
        ( id, newPosition )


movement : Time -> Game -> Game
movement dt game =
    let
        positions =
            Components.all1 game .cPosition
                |> List.map (moveEntity game dt)
                |> Dict.fromList
    in
        { game | cPosition = positions }
