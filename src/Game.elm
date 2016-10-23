module Game exposing (..)

import Algebra exposing (..)
import Array exposing (Array)
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


-- Colorations


colorations : Array Coloration
colorations =
    Array.fromList
        -- bright, dark
        [ ( "#f00", "#900", "red" )
        , ( "#0f0", "#090", "green" )
        , ( "#00f", "#009", "blue" )
        , ( "#0ee", "#0bb", "cyan" )
        , ( "#f0f", "#909", "purple" )
        , ( "#ee0", "#bb0", "yellow" )
        ]



-- state and outcome helpers


infixl 0 |>>
(|>>) : ( a, List c ) -> (a -> ( b, List c )) -> ( b, List c )
(|>>) ( oldModel, oldEffects ) f =
    let
        ( newModel, newEffects ) =
            f oldModel
    in
        ( newModel, oldEffects ++ newEffects )


foldOverList : (a -> b -> (b, List Outcome)) -> List a -> ( b, List Outcome ) -> ( b, List Outcome )
foldOverList f list accumulators =
    let
        folder value ( oldAccumulator, existingOutcomes ) =
            let
                ( newAccumulator, newOutcomes ) =
                    f value oldAccumulator
            in
                ( newAccumulator, existingOutcomes ++ newOutcomes )
    in
        List.foldl folder accumulators list


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



-- Models
{- This describes what a player wants their ship to do -}


type alias Model =
    { nextId : Int
    , shipsByName : Dict String Ship
    , playersByName : Dict String Player
    , projectiles : List Projectile
    , planets : List Planet
    , colorations : Array Coloration
    , lastUpdateTime : Time
    , seed : Random.Seed
    }



-- Init


init seed =
    { nextId = 1
    , shipsByName = Dict.empty
    , playersByName = Dict.empty
    , projectiles = []
    , planets = fst <| Random.step Planet.planetsGenerator seed
    , lastUpdateTime = 0
    , seed = seed
    }



-- Ship factories


makeShip : Player -> Vector -> String -> Ship
makeShip player position name =
    { playerName = player.name
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


shipNameGenerator : Player -> Random.Generator String
shipNameGenerator player =
    let
        (_, _, colorName) = player.coloration

        filler key =
            "---"
            --"{" ++ colorName ++ "}"
    in
        Names.ship filler



randomShip : Player -> Random.Generator Ship
randomShip player =
    Random.map2
        (makeShip player)
        randomPosition
        (shipNameGenerator player)


addShip : Player -> Model -> Model
addShip player model =
    let
        ( newShip, newSeed ) =
            Random.step (randomShip player) model.seed
    in
        { model
            | shipsByName = Dict.insert player.name newShip model.shipsByName
            , seed = newSeed
        }



-- Ship Tick


newProjectile ship =
    { playerId = ship.playerId
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
                  , E <| (ShipFires ship.playerName)
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
        newPosition =
            let
                -- Reduce speed if not moving straight ahead
                f =
                    0.85 + 0.15 * cos (vectorToAngle ship.velocityControl - ship.heading)
            in
                ship.position
                    |> V.add (V.scale (f * Ship.speed * dt) ship.velocityControl)
                    |> clampToRadius worldRadius

        targetHeading =
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
            , [ E <| ShipActivates oldShip.playerName
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
            , [ D <| RemoveShip oldShip.playerName ]
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



-- Projectile


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
            if ship.playerName /= oldProjectile.playerName && ship.status == Active && collisionWithShip ship then
                [ D <| RemoveProjectile newProjectile.id
                , D <| DamageShip ship.name
                , E <| ShipExplodes ship.name
                , E <| ShipDamagesShip oldProjectile.playerName ship.playerName
                ]
                    ++ deltas
            else
                deltas

        collisionEffets =
            Dict.foldl collideWithShip [] model.shipsByName

        boundaryEffects =
            if V.length newPosition > worldRadius then
                [ D <| RemoveProjectile newProjectile.id ]
            else
                []
    in
        ( newProjectile, collisionEffets ++ boundaryEffects )



-- Planets


tickPlanet : Time -> Planet -> Planet
tickPlanet dt planet =
    -- TODO: Animate satellites
    { planet | angle = normalizeAngle <| planet.angle + dt * planet.angularSpeed }



-- Players






        -- TODO apply controls
        -- TODO: ensure all *active* players have ships
--         playersAndControls
--             case ship 
--                 Nothing -> spawn ship
--                 Just ship ->
--                     if ship active then apply controls

applyControl : (Player, Control) -> Model -> ( Dict String Player, List Outcome )
applyControl (oldPlayer, control) model =
    let
        newPlayer =
            { oldPlayer
                | lastControl = model.lastUpdateTime
                , control = Just control
            }

        outcomes =
            case Dict.get newPlayer.name model.shipsByName of
                Nothing -> [D <| AddShip newPlayer]
                Just ship -> []

    in
        (playersByName, outcome)






tickPlayers : Time -> Dict String Control -> Dict String Player -> ( Dict String Player, List Outcome )
tickPlayers currentTime controlsByName playersByName =
    let

        folder playerName player ( playersAndControls, playersWithoutControls, remainingControlsByName ) =
            case Dict.get playerName remainingControlsByName of
                Nothing ->
                    ( playersAndControls, player :: playersWithoutControls, remainingControlsByName )
                Just control ->
                    ( (player, control) :: playersAndControls , playersWithoutControls, Dict.remove playerName remainingControlsByName )

        ( playersAndControls, playersWithoutControls, controlsWithoutPlayers ) =
            Dict.foldl folder ( [], [], controlsByName ) playersByName


{-
        


        -- TODO: remove ships of inactive players
        playersWithoutControls
            check last control time for *ship* removal

        -- TODO ensure every control has a player
        controlsWithoutPlayers
            add players
-}


--     applyControls : (Player, Control) -> Dict String Player -> ( Dict String Player, List Outcome )






    in
        (playersByName, [])
            |>> foldOverList (applyControl currentTime) playersAndControls
--             |>> removeShipsOfInactivePlayers playersWithoutControls
--             |>> addNewPlayers controlsWithoutPlayers








-- Main stuff


updateShip : Ship -> Model -> Model
updateShip ship model =
    { model | shipsByName = Dict.insert ship.playerName ship model.shipsByName }


applyDelta : Delta -> Model -> Model
applyDelta effect model =
    case effect of
        AddProjectile projectile ->
            { model | projectiles = projectile :: model.projectiles }

        RemoveProjectile projectile ->
            { model | projectiles = List.Extra.remove projectile model.projectiles }

        DamageShip playerName ->
            case Dict.get playerName model.shipsByName of
                Just ship ->
                    updateShip { ship | status = Exploding } model

                Nothing ->
                    model

        RemoveShip playerName ->
            { model | shipsByName = Dict.remove playerName model.shipsByName }


splitOutcomes : List Outcome -> ( List Delta, List Event )
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










tick : Time -> Dict String Control -> Model -> ( Model, List Event )
tick dt controlsByName oldModel =
    let
        newLastUpdatedTime =
            oldModel.lastUpdateTime + dt

        ( tickedPlayersByName, playerOutcomes ) =
            tickPlayers newLastUpdatedTime controlsByName oldModel.playersByName

        ( tickedshipsByName, outcomes1 ) =
            outcomesOverDict (shipTick tickedPlayersByName dt) oldModel.shipsByName

        ( tickedProjectiles, outcomes2 ) =
            outcomesOverList (projectileTick oldModel dt) oldModel.projectiles

        tickedPlanets =
            List.map (tickPlanet dt) oldModel.planets

        tickedModel =
            { oldModel
                | shipsByName = tickedshipsByName
                , playersByName = tickedPlayersByName
                , projectiles = tickedProjectiles
                , planets = tickedPlanets
            }

        ( deltas, events ) =
            splitOutcomes <| outcomes1 ++ outcomes2

        newModel =
            List.foldl applyDelta tickedModel deltas
    in
        ( newModel, events )


type Msg
    = Tick Time (Dict String Control)


update : Msg -> Model -> ( Model, List Event )
update msg model =
    case msg of
        Tick dt controlsByName ->
            tick dt controlsByName model
