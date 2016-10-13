module Game exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as V
import Names
import Time exposing (Time)
import Random


-- Global constants


starSystemOuterRadius =
    1.0


explosionDuration =
    1000 * Time.millisecond


spawnDuration =
    0 * Time.second


shipSpeed =
    0.3 * starSystemOuterRadius / Time.second


turningRate =
    turns 1 / Time.second


velocityControlThreshold =
    0.1


headingControlThreshold =
    0.3



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


clampToRadius radius v =
    let
        ll =
            V.lengthSquared v
    in
        if ll <= radius then
            v
        else
            V.scale (radius / sqrt ll) v


vectorToAngle v =
    let
        ( x, y ) =
            V.toTuple v
    in
        atan2 y x


normalizeAngle a =
    if a < -pi then
        a + 2 * pi
    else if a > pi then
        a - 2 * pi
    else
        a



-- Game Effects


type Effect
    = SpawnProjectile ()
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
    , position : Vector
    , heading : Float
    , status : Status
    , name : String
    }



-- Main Game Model


type alias Model =
    { shipsById : Dict Int Ship
    , seed : Random.Seed
    }



-- Init


init seed =
    Model Dict.empty (Random.initialSeed seed)



-- Game logic


shipFireControl : Time -> ActiveModel -> Ship -> ( Ship, List Effect )
shipFireControl dt activeModel ship =
    ( ship, [] )


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
                        |> clampToRadius starSystemOuterRadius

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
            turningRate * dt

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
        (Random.float 0 starSystemOuterRadius)
        (Random.float 0 (turns 1))


makeShip controllerId position name =
    { controllerId = controllerId
    , velocityControl = v0
    , headingControl = v0
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


applyEffect : Effect -> Model -> Model
applyEffect effect model =
    case effect of
        SpawnProjectile _ ->
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

        ( tickedShipsById, effects ) =
            Dict.foldl folder ( Dict.empty, [] ) oldModel.shipsById

        newModel =
            List.foldl applyEffect { oldModel | shipsById = tickedShipsById } effects
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
            updateShip { ship | velocityControl = velocity, headingControl = heading } model

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
