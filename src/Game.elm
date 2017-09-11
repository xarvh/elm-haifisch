module Game exposing (..)

import Array exposing (Array)
import ColorPattern exposing (ColorPattern)
import Collision
import Common exposing (..)
import Dict exposing (Dict)
import List.Extra
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Names
import Planet
import Ship
import Time exposing (Time)
import Random
import Random.Array


-- Main Game Model


type alias Model =
    { planets : List Planet
    , players : List Player
    , projectiles : List Projectile
    , seed : Random.Seed
    , shipsById : Dict Int Ship
    , shuffledColorPatterns : Array ColorPattern
    }



-- Global constants


worldRadius =
    Common.worldRadius


planetAngularVelocity =
    turns 0.05 / Time.second


satelliteAngularVelocity =
    turns 0.09 / Time.second


velocityControlThreshold =
    0.1


headingControlThreshold =
    0.3


projectileSpeed =
    1 * worldRadius / Time.second



-- state and outcome helpers


infixl 0 |>>
(|>>) : ( a, List c ) -> (a -> ( b, List c )) -> ( b, List c )
(|>>) ( oldModel, oldEffects ) f =
    let
        ( newModel, newEffects ) =
            f oldModel
    in
        ( newModel, oldEffects ++ newEffects )


outcomesOverDict : (v -> ( v, List o )) -> Dict comparable v -> ( Dict comparable v, List o )
outcomesOverDict f dict =
    let
        folder key oldValue ( dict, oldOutcomes ) =
            let
                ( newValue, newOutcomes ) =
                    f oldValue
            in
                ( Dict.insert key newValue dict, oldOutcomes ++ newOutcomes )
    in
        Dict.foldl folder ( Dict.empty, [] ) dict


outcomesOverList : (v -> ( v, List o )) -> List v -> ( List v, List o )
outcomesOverList f oldList =
    let
        folder oldValue ( list, oldOutcomes ) =
            let
                ( newValue, newOutcomes ) =
                    f oldValue
            in
                ( newValue :: list, oldOutcomes ++ newOutcomes )
    in
        List.foldl folder ( [], [] ) oldList



-- Init


init seed =
    { planets = Tuple.first <| Random.step Planet.planetsGenerator seed
    , players = []
    , projectiles = []
    , seed = seed
    , shipsById = Dict.empty
    , shuffledColorPatterns = Random.step (Random.Array.shuffle ColorPattern.patterns) seed |> Tuple.first
    }



-- Control


newProjectile ship =
    { playerId = ship.playerId
    , position = ship.position
    , heading = ship.heading
    }


shipFireControl : InputState -> Time -> Ship -> ( Ship, List Outcome )
shipFireControl inputState dt ship =
    let
        ( newReloadTime, deltas ) =
            if inputState.fire && ship.reloadTime == 0 then
                ( Ship.reloadTime
                , [ D <| AddProjectile (newProjectile ship)
                  , E <| (ShipFires ship.playerId)
                  ]
                )
            else
                ( max 0 (ship.reloadTime - dt), [] )

        newShip =
            { ship | reloadTime = newReloadTime }
    in
        ( newShip, deltas )


shipMovementControl : InputState -> Time -> Ship -> ( Ship, List Outcome )
shipMovementControl inputState dt ship =
    let
        ignoreVelocityControl =
            Vec2.length inputState.move < velocityControlThreshold

        ignoreHeadingControl =
            Vec2.length inputState.finalAim < headingControlThreshold

        newPosition =
            if ignoreVelocityControl then
                ship.position
            else
                let
                    -- Reduce speed if not moving straight ahead
                    f =
                        0.85 + 0.15 * cos (vectorToAngle inputState.move - ship.heading)
                in
                    ship.position
                        |> Vec2.add (Vec2.scale (f * Ship.speed * dt) inputState.move)
                        |> clampToRadius worldRadius

        targetHeading =
            if ignoreHeadingControl then
                if ignoreVelocityControl then
                    ship.heading
                else
                    vectorToAngle inputState.move
            else
                vectorToAngle inputState.finalAim

        deltaHeading =
            normalizeAngle <| targetHeading - ship.heading

        maxTurn =
            Ship.turningRate * dt

        clampedDeltaAngle =
            clamp -maxTurn maxTurn deltaHeading

        newHeading =
            normalizeAngle <| ship.heading + clampedDeltaAngle
    in
        ( { ship | position = newPosition, heading = newHeading }, [] )



-- Ship factories


makeShip playerId position name =
    { playerId = playerId
    , heading = vectorToAngle <| Vec2.negate position
    , position = position
    , name = name
    , status = Spawning
    , reloadTime = 0
    , respawnTime = 0
    , explodeTime = 0
    }


randomPosition : Random.Generator Vec2
randomPosition =
    Random.map2
        (\r a -> vec2 (r * sin a) (r * cos a))
        (Random.float 0 worldRadius)
        (Random.float 0 (turns 1))


randomShip : Int -> String -> Random.Generator Ship
randomShip playerId colorKey =
    Random.map2
        (makeShip playerId)
        randomPosition
        (Names.ship colorKey)


addShip : Int -> String -> Model -> Model
addShip playerId colorKey model =
    let
        ( newShip, newSeed ) =
            Random.step (randomShip playerId colorKey) model.seed
    in
        { model
            | shipsById = Dict.insert playerId newShip model.shipsById
            , seed = newSeed
        }



-- Ship


shipSpawnTick : Time -> Ship -> ( Ship, List Outcome )
shipSpawnTick dt oldShip =
    let
        newRespawnTime =
            oldShip.respawnTime + dt
    in
        if newRespawnTime > Ship.spawnDuration then
            ( { oldShip
                | status = Active
                , reloadTime = 0
              }
            , [ E <| ShipActivates oldShip.playerId
              ]
            )
        else
            ( { oldShip
                | respawnTime = newRespawnTime
              }
            , []
            )


shipExplodeTick : Time -> Ship -> ( Ship, List Outcome )
shipExplodeTick dt oldShip =
    let
        newExplodeTime =
            oldShip.explodeTime + dt
    in
        if newExplodeTime >= Ship.explosionDuration then
            ( oldShip
            , [ D <| RemoveShip oldShip.playerId ]
            )
        else
            ( { oldShip | explodeTime = min Ship.explosionDuration newExplodeTime }
            , []
            )


shipTick : Dict Id InputState -> Time -> Ship -> ( Ship, List Outcome )
shipTick inputStateByPlayerId dt ship =
    case Dict.get ship.playerId inputStateByPlayerId of
        Nothing ->
            shipExplodeTick dt ship

        Just inputState ->
            case ship.status of
                Spawning ->
                    ship
                        |> shipMovementControl inputState dt
                        |>> shipSpawnTick dt

                Active ->
                    ship
                        |> shipMovementControl inputState dt
                        |>> shipFireControl inputState dt

                Exploding ->
                    ship
                        |> shipExplodeTick dt



-- Projectiles


projectileTick : Model -> Time -> Projectile -> ( Projectile, List Outcome )
projectileTick model dt oldProjectile =
    let
        newPosition =
            Vec2.add oldProjectile.position <| Vec2.scale (projectileSpeed * dt) (angleToVector oldProjectile.heading)

        newProjectile =
            { oldProjectile | position = newPosition }

        collisionWithShip ship =
            Collision.projectileVsShip oldProjectile.position newProjectile.position ship

        collideWithShip id ship deltas =
            if ship.playerId /= oldProjectile.playerId && ship.status == Active && collisionWithShip ship then
                [ D <| RemoveProjectile newProjectile
                , D <| DamageShip ship.playerId
                , E <| ShipExplodes ship.playerId
                , E <| ShipDamagesShip oldProjectile.playerId ship.playerId
                ]
                    ++ deltas
            else
                deltas

        collisionEffets =
            Dict.foldl collideWithShip [] model.shipsById

        boundaryEffects =
            if Vec2.length newPosition > worldRadius then
                [ D <| RemoveProjectile newProjectile ]
            else
                []
    in
        ( newProjectile, collisionEffets ++ boundaryEffects )



-- Planets


satelliteTick : Time -> Satellite -> Satellite
satelliteTick dt satellite =
    { satellite | angle = satellite.angle + dt * satellite.angularSpeed }


planetTick : Time -> Planet -> Planet
planetTick dt planet =
    let
        newAngle =
            normalizeAngle <| planet.angle + dt * planet.angularSpeed

        newSatellites =
            List.map (satelliteTick dt) planet.satellites
    in
        { planet | angle = newAngle, satellites = newSatellites }



-- Players


addPlayer : Model -> ( Model, Player )
addPlayer model =
    let
        lastId =
            model.players
                |> List.map .id
                |> List.maximum
                |> Maybe.withDefault 0

        newId =
            lastId + 1

        player =
            { id = newId
            , score = 0
            , colorPattern = ColorPattern.get newId model.shuffledColorPatterns
            }

        players =
            player :: model.players

        newModel =
            addShip player.id player.colorPattern.key model
    in
        ( { newModel | players = players }, player )



-- Main state stuff


updateShip : Ship -> Model -> Model
updateShip ship model =
    { model | shipsById = Dict.insert ship.playerId ship model.shipsById }


applyDelta : Delta -> Model -> Model
applyDelta effect model =
    case effect of
        AddProjectile projectile ->
            { model | projectiles = projectile :: model.projectiles }

        RemoveProjectile projectile ->
            { model | projectiles = List.Extra.remove projectile model.projectiles }

        DamageShip playerId ->
            case Dict.get playerId model.shipsById of
                Just ship ->
                    updateShip { ship | status = Exploding } model

                Nothing ->
                    model

        RemoveShip playerId ->
            { model | shipsById = Dict.remove playerId model.shipsById }


splitOutcomes outcomes =
    let
        folder outcome ( deltas, events ) =
            case outcome of
                D delta ->
                    ( delta :: deltas, events )

                E event ->
                    ( deltas, event :: events )
    in
        List.foldl folder ( [], [] ) outcomes


tick : Dict Id InputState -> Time -> Model -> ( Model, List Event )
tick inputStateByPlayerId dt oldModel =
    let
        ( tickedShipsById, shipOutcomes ) =
            outcomesOverDict (shipTick inputStateByPlayerId dt) oldModel.shipsById

        ( tickedProjectiles, projectileOutcomes ) =
            outcomesOverList (projectileTick oldModel dt) oldModel.projectiles

        tickedPlanets =
            List.map (planetTick dt) oldModel.planets

        tickedModel =
            { oldModel
                | shipsById = tickedShipsById
                , projectiles = tickedProjectiles
                , planets = tickedPlanets
            }

        ( deltas, events ) =
            splitOutcomes <| shipOutcomes ++ projectileOutcomes

        newModel =
            List.foldl applyDelta tickedModel deltas
    in
        ( newModel, events )
