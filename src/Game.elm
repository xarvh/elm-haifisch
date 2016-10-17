module Game exposing (..)

import Array
import Dict exposing (Dict)
import List.Extra
import Math.Vector2 as V
import Names
import Time exposing (Time)
import Random


-- Global constants
-- This is used as unit measure of length


worldRadius =
    1.0


explosionDuration =
    1000 * Time.millisecond


spawnDuration =
    3 * Time.second


shipSpeed =
    0.3 * worldRadius / Time.second


shipTurningRate =
    turns 1 / Time.second


velocityControlThreshold =
    0.1


headingControlThreshold =
    0.3


fireReloadTime =
    0.3 * Time.second


projectileSpeed =
    1 * worldRadius / Time.second



{-
   Ship vertexes, clockwise from the rear
    ___
    \  --__
    /__--

-}


shipTotalLength =
    0.06 * worldRadius


shipMesh =
    List.map
        (V.fromTuple >> V.scale (shipTotalLength / 15))
        [ ( -3, 0 )
        , ( -5, 5 )
        , ( 10, 0 )
        , ( -5, -5 )
        ]


shipConvexMesh =
    List.drop 1 shipMesh



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



-- Linear Algebra helpers


type alias Vector =
    V.Vec2


vector =
    V.vec2


v0 =
    V.vec2 0 0


vectorToString : Vector -> String
vectorToString v =
    toString (V.getX v) ++ "," ++ toString (V.getY v)


clampToRadius : Float -> Vector -> Vector
clampToRadius radius v =
    let
        ll =
            V.lengthSquared v
    in
        if ll <= radius then
            v
        else
            V.scale (radius / sqrt ll) v


vectorToAngle : Vector -> Float
vectorToAngle v =
    let
        ( x, y ) =
            V.toTuple v
    in
        atan2 y x


angleToVector : Float -> Vector
angleToVector a =
    vector (cos a) (sin a)


normalizeAngle : Float -> Float
normalizeAngle a =
    if a < -pi then
        a + 2 * pi
    else if a > pi then
        a - 2 * pi
    else
        a



-- TODO: this normalizes only to within [-pi, +pi], rather than [-pi/2, +pi/2]
--
-- normalizeAngle : Float -> Float
-- normalizeAngle a =
--     let
--         turnsToRemove =
--             truncate <| a / (turns 1)
--     in
--         a - (turnsToRemove * turns 1)


rightHandNormal : Vector -> Vector
rightHandNormal v =
    let
        ( x, y ) =
            V.toTuple v
    in
        vector -y x


rotateVector : Float -> Vector -> Vector
rotateVector angle v =
    let
        ( x, y ) =
            V.toTuple v

        sinA =
            sin angle

        cosA =
            cos angle
    in
        vector
            (x * cosA - y * sinA)
            (x * sinA + y * cosA)



-- Collision detection
-- TODO: Needs some optimization


type alias Segment =
    ( Vector, Vector )


type alias Polygon =
    List Vector


anySegment : (( Vector, Vector ) -> Bool) -> Polygon -> Bool
anySegment f poly =
    let
        a =
            Array.fromList poly

        get index =
            Array.get (index % (Array.length a)) a |> Maybe.withDefault (vector 0 0)

        segments =
            List.indexedMap (\index v -> ( get index, get (index + 1) )) poly
    in
        List.any f segments


normalIsSeparatingAxis q ( a, b ) =
    let
        n =
            rightHandNormal <| V.sub b a

        isRightSide p =
            V.dot n (V.sub p a) > 0
    in
        List.all isRightSide q


halfCollision : Polygon -> Polygon -> Bool
halfCollision p q =
    -- https://www.toptal.com/game/video-game-physics-part-ii-collision-detection-for-solid-objects
    -- Try polygon p's normals as separating axies.
    -- If any of them does separe the polys, then the two polys are NOT intersecting
    not <| anySegment (normalIsSeparatingAxis q) p


collisionPolygonVsPolygon : Polygon -> Polygon -> Bool
collisionPolygonVsPolygon p q =
    halfCollision p q && halfCollision q p


collisionSegmentVsPolygon ( a, b ) p =
    collisionPolygonVsPolygon [ a, b ] p



-- Deltas describe generic changes in the game model


type Delta
    = AddProjectile Projectile
    | RemoveProjectile Projectile
    | DamageShip Int
    | RemoveShip Int



-- Events are used to notify the parent update of significant events happening within the game.
-- For example, play sounds or update score.


type Event
    = ShipExplodes Int
    | ShipFires Int
    | ShipAppears Int
    | ShipActivates Int
    | ShipDamagesShip Int Int


type Outcome
    = D Delta
    | E Event



-- Ships


type Status
    = Spawning
    | Active
    | Exploding


type alias Ship =
    { controllerId : Int
    , velocityControl : Vector
    , headingControl : Vector
    , fireControl : Bool
    , position : Vector
    , heading : Float
    , status : Status
    , name : String
    , reloadTime : Time
    , explodeTime : Time
    , respawnTime : Time
    }


type alias Projectile =
    { ownerControllerId : Int
    , position : Vector
    , heading : Float
    }


shipTransform : Ship -> Polygon -> Polygon
shipTransform ship polygon =
    List.map (rotateVector ship.heading >> V.add ship.position) polygon



-- Main Game Model


type alias Model =
    { shipsById : Dict Int Ship
    , projectiles : List Projectile
    , seed : Random.Seed
    }



-- Init


init seed =
    { shipsById = Dict.empty
    , projectiles = []
    , seed = seed
    }



-- Game logic


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
                ( fireReloadTime
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
                        |> V.add (V.scale (f * shipSpeed * dt) ship.velocityControl)
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
            shipTurningRate * dt

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


randomShip : Int -> Random.Generator Ship
randomShip controllerId =
    Random.map2
        (makeShip controllerId)
        randomPosition
        Names.ship


addShip : Int -> Model -> Model
addShip controllerId model =
    let
        ( newShip, newSeed ) =
            Random.step (randomShip controllerId) model.seed
    in
        { model
            | shipsById = Dict.insert controllerId newShip model.shipsById
            , seed = newSeed
        }



-- Tick


shipSpawnTick : Time -> Ship -> ( Ship, List Outcome )
shipSpawnTick dt oldShip =
    let
        newRespawnTime =
            oldShip.respawnTime + dt
    in
        if newRespawnTime > spawnDuration then
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
        if newExplodeTime >= explosionDuration then
            ( oldShip
            , [ D <| RemoveShip oldShip.controllerId ]
            )
        else
            ( { oldShip | explodeTime = min explosionDuration newExplodeTime }
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


projectileTick : Model -> Time -> Projectile -> ( Projectile, List Outcome )
projectileTick model dt oldProjectile =
    let
        newPosition =
            V.add oldProjectile.position <| V.scale (projectileSpeed * dt) (angleToVector oldProjectile.heading)

        newProjectile =
            { oldProjectile | position = newPosition }

        collisionWithShip ship =
            collisionSegmentVsPolygon ( oldProjectile.position, newProjectile.position ) (shipTransform ship shipConvexMesh)

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
                D delta -> ( delta :: deltas, events )
                E event -> ( deltas, event :: events )
    in
        List.foldl folder ( [], []) outcomes


tick : Time -> Model -> ( Model, List Event )
tick dt oldModel =
    let
        ( tickedShipsById, shipOutcomes ) =
            outcomesOverDict (shipTick dt) oldModel.shipsById

        ( tickedProjectiles, projectileOutcomes ) =
            outcomesOverList (projectileTick oldModel dt) oldModel.projectiles

        tickedModel =
            { oldModel
                | shipsById = tickedShipsById
                , projectiles = tickedProjectiles
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
    | AddShip Int
    | Tick Time


updateShip : Ship -> Model -> Model
updateShip ship model =
    { model | shipsById = Dict.insert ship.controllerId ship model.shipsById }


noEvents m =
    ( m, [] )


update : Msg -> Model -> ( Model, List Event )
update msg model =
    case msg of
        AddShip controllerId ->
            noEvents <| addShip controllerId model

        ControlShip ship ( velocity, heading, isFiring ) ->
            noEvents <| updateShip { ship | velocityControl = velocity, headingControl = heading, fireControl = isFiring } model

        KillShip ship ->
            noEvents <| updateShip { ship | status = Exploding } model

        Tick dt ->
            tick dt model
