module Systems.Movement exposing (..)

import Common
import Components exposing (EntityId)
import Dict exposing (Dict)
import Game exposing (Game)
import Time exposing (Time)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


type alias Constraint =
    Vec2 -> Vec2


shipSpeed =
    7 * Common.shipLength / Time.second


stayInStarSystem : Constraint
stayInStarSystem =
    Common.clampToRadius Common.worldRadius


shipVelocity : Game -> ( EntityId, Game.ShipComponent ) -> Vec2
shipVelocity game ( id, ship ) =
    if ship.status == Game.ShipExploding then
        vec2 0 0
    else
        let
            baseVelocity =
                Game.shipOwner game id
                    |> Maybe.andThen .inputState
                    |> Maybe.map (.move >> Common.clampToRadius 1 >> Vec2.scale shipSpeed)
                    |> Maybe.withDefault (vec2 0 0)

            heading =
                Components.get game.cHeading id |> Maybe.withDefault 0

            -- Reduce speed if not moving straight ahead
            f =
                0.85 + 0.15 * cos (Common.vectorToAngle baseVelocity - heading)
        in
            Vec2.scale f baseVelocity


entityVelocity : Game -> EntityId -> ( Vec2, Constraint )
entityVelocity game id =
    case Components.get game.cShip id of
        Just ship ->
            ( shipVelocity game ( id, ship ), stayInStarSystem )

        Nothing ->
            -- TODO projectiles and planets
            ( vec2 0 0, identity )


moveEntity : Game -> Time -> ( EntityId, Vec2 ) -> ( EntityId, Vec2 )
moveEntity game dt ( id, x ) =
    let
        ( velocity, constraint ) =
            entityVelocity game id

        dx =
            Vec2.scale dt velocity

        newPosition =
            Vec2.add x dx |> constraint
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
