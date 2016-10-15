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
    0 * Time.second


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


thePoly =
    List.map (V.scale <| 0.1 * worldRadius)
        [ vector -1 -1
        , vector -1 1
        , vector 1 1
        , vector 1 -1
        ]



-- Game Effects


type Effect
    = SpawnProjectile Projectile
      -- TODO should use an id
    | RemoveProjectile Projectile
    | DamageShip Int
    | RemoveShip Int



-- Ships


type alias ActiveModel =
    { gunCooldown : Time
    }


type Status
    = Spawning Time
    | Active ActiveModel
    | Exploding Time


type alias Ship =
    { controllerId : Int
    , velocityControl : Vector
    , headingControl : Vector
    , fireControl : Bool
    , position : Vector
    , heading : Float
    , status : Status
    , name : String
    }


type alias Projectile =
    { ownerControllerId : Int
    , position : Vector
    , heading : Float
    }


shipPolygon : Ship -> Polygon
shipPolygon ship =
    List.map (rotateVector ship.heading >> V.add ship.position) shipMesh



-- Main Game Model


type alias Model =
    { shipsById : Dict Int Ship
    , projectiles : List Projectile
    , colorOffset : Int
    , seed : Random.Seed
    }



-- Init


init newDate =
    let
        seed =
            Random.initialSeed newDate
    in
        { shipsById = Dict.empty
        , projectiles = []
        , colorOffset = fst <| Random.step (Random.int 0 Random.maxInt) seed
        , seed = seed
        }



-- Game logic


newProjectile ship =
    { ownerControllerId = ship.controllerId
    , position = ship.position
    , heading = ship.heading
    }


shipFireControl : Time -> ActiveModel -> Ship -> ( Ship, List Effect )
shipFireControl dt { gunCooldown } ship =
    let
        ( cooldown, effects ) =
            if ship.fireControl && gunCooldown == 0 then
                ( fireReloadTime, [ SpawnProjectile (newProjectile ship) ] )
            else
                ( max 0 (gunCooldown - dt), [] )

        newStatus =
            Active { gunCooldown = cooldown }
    in
        ( { ship | status = newStatus }, effects )


shipMovementControl : Time -> Ship -> Ship
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
        { ship | position = newPosition, heading = newHeading }


randomPosition : Random.Generator Vector
randomPosition =
    Random.map2
        (\r a -> vector (r * sin a) (r * cos a))
        (Random.float 0 worldRadius)
        (Random.float 0 (turns 1))


makeShip controllerId position name =
    { controllerId = controllerId
    , velocityControl = v0
    , headingControl = v0
    , fireControl = False
    , heading = vectorToAngle <| V.negate position
    , position = position
    , name = name
    , status = Spawning 0
    }


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


noEffect ship =
    ( ship, [] )


shipSpawnTick : Time -> Time -> Ship -> Ship
shipSpawnTick dt oldElapsedTime ship =
    let
        newElapsedTime =
            oldElapsedTime + dt

        newStatus =
            if newElapsedTime > spawnDuration then
                Active { gunCooldown = 0 }
            else
                Spawning newElapsedTime
    in
        { ship | status = newStatus }


shipExplodeTick : Time -> Time -> Ship -> ( Ship, List Effect )
shipExplodeTick dt oldElapsedTime ship =
    let
        newElapsedTime =
            oldElapsedTime + dt

        newStatus =
            Exploding <| min explosionDuration newElapsedTime

        effects =
            if newElapsedTime >= explosionDuration then
                [ RemoveShip ship.controllerId ]
            else
                []
    in
        ( { ship | status = newStatus }, effects )


shipTick : Time -> Ship -> ( Ship, List Effect )
shipTick dt ship =
    case ship.status of
        -- TODO: allow the ship to move during spawn
        Spawning oldElapsedTime ->
            ship
                |> shipMovementControl dt
                |> shipSpawnTick dt oldElapsedTime
                |> noEffect

        Active activeModel ->
            ship
                |> shipMovementControl dt
                |> shipFireControl dt activeModel

        Exploding elapsedTime ->
            ship
                |> shipExplodeTick dt elapsedTime


-- I resent the need for this helper
-- TODO: find a better way
isActive ship =
    case ship.status of
        Active _ ->
            True

        _ ->
            False


projectileTick : Model -> Time -> Projectile -> ( Projectile, List Effect )
projectileTick model dt projectile =
    let
        newPosition =
            V.add projectile.position <| V.scale (projectileSpeed * dt) (angleToVector projectile.heading)

        newProjectile =
            { projectile | position = newPosition }

        collisionWithShip ship =
            collisionSegmentVsPolygon ( newPosition, projectile.position ) (shipPolygon ship)

        collideWithShip id ship effects =
            if ship.controllerId /= projectile.ownerControllerId && isActive ship && collisionWithShip ship then
                RemoveProjectile newProjectile :: DamageShip ship.controllerId :: effects
            else
                effects

        collisionEffets =
            Dict.foldl collideWithShip [] model.shipsById

        boundaryEffects =
            if V.length newPosition > worldRadius then
                [ RemoveProjectile newProjectile ]
            else
                []
    in
        ( newProjectile, collisionEffets ++ boundaryEffects )


applyEffect : Effect -> Model -> Model
applyEffect effect model =
    case effect of
        SpawnProjectile projectile ->
            { model | projectiles = projectile :: model.projectiles }

        RemoveProjectile projectile ->
            { model | projectiles = List.Extra.remove projectile model.projectiles }

        DamageShip controllerId ->
            case Dict.get controllerId model.shipsById of
                Just ship ->
                    updateShip { ship | status = Exploding 0 } model

                Nothing ->
                    model

        RemoveShip controllerId ->
            { model | shipsById = Dict.remove controllerId model.shipsById }


tick : Time -> Model -> Model
tick dt oldModel =
    let
        folder id oldShip ( shipsById, effects ) =
            let
                ( newShip, newEffects ) =
                    shipTick dt oldShip
            in
                ( Dict.insert id newShip shipsById, newEffects ++ effects )

        ( tickedShipsById, shipEffects ) =
            Dict.foldl folder ( Dict.empty, [] ) oldModel.shipsById

        projectileFolder oldProj ( projs, effects ) =
            let
                ( newProjectile, newEffects ) =
                    projectileTick oldModel dt oldProj
            in
                ( newProjectile :: projs, newEffects ++ effects )

        ( tickedProjectiles, projectileEffects ) =
            List.foldl projectileFolder ( [], [] ) oldModel.projectiles

        tickedModel =
            { oldModel
                | shipsById = tickedShipsById
                , projectiles = tickedProjectiles
            }

        newModel =
            List.foldl applyEffect tickedModel (shipEffects ++ projectileEffects)
    in
        newModel



-- Game Msg


type Msg
    = ControlShip Ship ( Vector, Vector, Bool )
    | KillShip Ship
    | AddShip Int
    | Tick Time


updateShip : Ship -> Model -> Model
updateShip ship model =
    { model | shipsById = Dict.insert ship.controllerId ship model.shipsById }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddShip controllerId ->
            addShip controllerId model

        ControlShip ship ( velocity, heading, isFiring ) ->
            updateShip { ship | velocityControl = velocity, headingControl = heading, fireControl = isFiring } model

        KillShip ship ->
            let
                -- TODO is there a better do this pattern matching?
                newStatus =
                    case ship.status of
                        Exploding elapsedTime ->
                            ship.status

                        _ ->
                            Exploding 0
            in
                updateShip { ship | status = newStatus } model

        Tick dt ->
            tick dt model
