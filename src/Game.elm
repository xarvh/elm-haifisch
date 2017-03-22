module Game exposing (..)

import Array
import Collision
import Common exposing (..)
import Dict exposing (Dict)
import List.Extra
import Math.Vector2 as V
import Names
import Planet
import Ship
import Time exposing (Time)
import Random


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



-- Main Game Model


type alias Model =
    { shipsById : Dict Int Ship
    , projectiles : List Projectile
    , planets : List Planet
    , seed : Random.Seed
    }



-- Init


init seed =
    { shipsById = Dict.empty
    , projectiles = []
    , seed = seed
    , planets = Tuple.first <| Random.step Planet.planetsGenerator seed
    }



-- Control


newProjectile ship =
    { ownerControllerId = ship.controllerId
    , position = ship.position
    , heading = ship.heading
    }


shipFireControl : Time -> Ship -> ( Ship, List Outcome )
shipFireControl dt ship =
    let
        ( newReloadTime, deltas ) =
            if ship.fireControl && ship.reloadTime == 0 then
                ( Ship.reloadTime
                , [ D <| AddProjectile (newProjectile ship)
                  , E <| (ShipFires ship.controllerId)
                  ]
                )
            else
                ( max 0 (ship.reloadTime - dt), [] )

        newShip =
            { ship | reloadTime = newReloadTime }
    in
        ( newShip, deltas )


shipMovementControl : Time -> Ship -> ( Ship, List Outcome )
shipMovementControl dt ship =
    let
        ignoreVelocityControl =
            V.length ship.velocityControl < velocityControlThreshold

        ignoreHeadingControl =
            V.length ship.headingControl < headingControlThreshold

        newPosition =
            if ignoreVelocityControl then
                ship.position
            else
                let
                    -- Reduce speed if not moving straight ahead
                    f =
                        0.85 + 0.15 * cos (vectorToAngle ship.velocityControl - ship.heading)
                in
                    ship.position
                        |> V.add (V.scale (f * Ship.speed * dt) ship.velocityControl)
                        |> clampToRadius worldRadius

        targetHeading =
            if ignoreHeadingControl then
                if ignoreVelocityControl then
                    ship.heading
                else
                    vectorToAngle ship.velocityControl
            else
                vectorToAngle ship.headingControl

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


makeShip controllerId position name =
    { controllerId = controllerId
    , velocityControl = v0
    , headingControl = v0
    , fireControl = False
    , heading = vectorToAngle <| V.negate position
    , position = position
    , name = name
    , status = Spawning
    , reloadTime = 0
    , respawnTime = 0
    , explodeTime = 0
    }


randomPosition : Random.Generator Vector
randomPosition =
    Random.map2
        (\r a -> vector (r * sin a) (r * cos a))
        (Random.float 0 worldRadius)
        (Random.float 0 (turns 1))


randomShip : Int -> String -> Random.Generator Ship
randomShip controllerId colorName =
    Random.map2
        (makeShip controllerId)
        randomPosition
        (Names.ship colorName)


addShip : Int -> String -> Model -> Model
addShip controllerId colorName model =
    let
        ( newShip, newSeed ) =
            Random.step (randomShip controllerId colorName) model.seed
    in
        { model
            | shipsById = Dict.insert controllerId newShip model.shipsById
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
            , [ E <| ShipActivates oldShip.controllerId
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
            , [ D <| RemoveShip oldShip.controllerId ]
            )
        else
            ( { oldShip | explodeTime = min Ship.explosionDuration newExplodeTime }
            , []
            )


shipTick : Time -> Ship -> ( Ship, List Outcome )
shipTick dt ship =
    case ship.status of
        Spawning ->
            ship
                |> shipMovementControl dt
                |>> shipSpawnTick dt

        Active ->
            ship
                |> shipMovementControl dt
                |>> shipFireControl dt

        Exploding ->
            ship
                |> shipExplodeTick dt



-- Projectiles


projectileTick : Model -> Time -> Projectile -> ( Projectile, List Outcome )
projectileTick model dt oldProjectile =
    let
        newPosition =
            V.add oldProjectile.position <| V.scale (projectileSpeed * dt) (angleToVector oldProjectile.heading)

        newProjectile =
            { oldProjectile | position = newPosition }

        collisionWithShip ship =
            Collision.projectileVsShip oldProjectile.position newProjectile.position ship

        collideWithShip id ship deltas =
            if ship.controllerId /= oldProjectile.ownerControllerId && ship.status == Active && collisionWithShip ship then
                [ D <| RemoveProjectile newProjectile
                , D <| DamageShip ship.controllerId
                , E <| ShipExplodes ship.controllerId
                , E <| ShipDamagesShip oldProjectile.ownerControllerId ship.controllerId
                ]
                    ++ deltas
            else
                deltas

        collisionEffets =
            Dict.foldl collideWithShip [] model.shipsById

        boundaryEffects =
            if V.length newPosition > worldRadius then
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



-- Main state stuff


applyDelta : Delta -> Model -> Model
applyDelta effect model =
    case effect of
        AddProjectile projectile ->
            { model | projectiles = projectile :: model.projectiles }

        RemoveProjectile projectile ->
            { model | projectiles = List.Extra.remove projectile model.projectiles }

        DamageShip controllerId ->
            case Dict.get controllerId model.shipsById of
                Just ship ->
                    updateShip { ship | status = Exploding } model

                Nothing ->
                    model

        RemoveShip controllerId ->
            { model | shipsById = Dict.remove controllerId model.shipsById }


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


tick : Time -> Model -> ( Model, List Event )
tick dt oldModel =
    let
        ( tickedShipsById, shipOutcomes ) =
            outcomesOverDict (shipTick dt) oldModel.shipsById

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



-- Game Msg


type Msg
    = ControlShip Ship ( Vector, Vector, Bool )
    | KillShip Ship
    | AddShip Int String
    | Tick Time


updateShip : Ship -> Model -> Model
updateShip ship model =
    { model | shipsById = Dict.insert ship.controllerId ship model.shipsById }


noEvents m =
    ( m, [] )


update : Msg -> Model -> ( Model, List Event )
update msg model =
    case msg of
        AddShip controllerId colorName ->
            noEvents <| addShip controllerId colorName model

        ControlShip ship ( velocity, heading, isFiring ) ->
            noEvents <| updateShip { ship | velocityControl = velocity, headingControl = heading, fireControl = isFiring } model

        KillShip ship ->
            noEvents <| updateShip { ship | status = Exploding } model

        Tick dt ->
            tick dt model
