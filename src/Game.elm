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
    , velocity : Vector
    , heading : Vector
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
    , velocity = v0
    , heading = V.negate <| V.normalize position
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


shipTick dt id ship =
    case ship.status of
        -- TODO: allow the ship to move during spawn
        Spawning oldElapsedTime ->
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

        Active activeModel ->
            let
                newPosition =
                    V.add ship.position <| V.scale (shipSpeed * dt) ship.velocityControl

                length =
                    V.length newPosition

                clampedPosition =
                    if length <= starSystemOuterRadius then
                        newPosition
                    else
                        V.scale (starSystemOuterRadius / length) newPosition
            in
                { ship | position = clampedPosition }

        Exploding elapsedTime ->
            { ship | status = Exploding <| max explosionDuration (elapsedTime + dt) }



-- Game Msg


type Msg
    = ControlShip Ship ( Vector, Vector, Bool )
    | RemoveShip Ship
    | AddShip Int
    | Tick Time


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddShip controllerId ->
            addShip controllerId model

        ControlShip ship ( velocity, heading, isFiring ) ->
            let
                newShip =
                    { ship | velocityControl = velocity, headingControl = heading }

                newShipsById =
                    Dict.insert ship.controllerId newShip model.shipsById
            in
                { model | shipsById = newShipsById }

        RemoveShip ship ->
            model

        Tick dt ->
            { model | shipsById = Dict.map (shipTick dt) model.shipsById }
