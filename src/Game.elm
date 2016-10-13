module Game exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as V
import Names
import Time exposing (Time)
import Random


-- Global constants


starSystemOuterRadius =
    1.0



-- Linear Algebra helpers


type alias Vector =
    V.Vec2


vector =
    V.vec2


vectorToString : Vector -> String
vectorToString v =
    toString (V.getX v) ++ "," ++ toString (V.getY v)



-- Ships


type alias ActiveModel =
    { velocity : Vector
    , gunCooldown : Time
    }


type Status
    = Spawining Float
    | Active ActiveModel
    | Exploding Float


type alias Ship =
    { controllerId : Int
    , position : Vector
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
    Model Dict.empty seed



-- initShip controllerId =
--     Ship controllerId (Ship.vector 0 0) (Ship.Spawining 0) ""
-- Game logic


randomPosition : Random.Generator Vector
randomPosition =
    Random.map2
        (\r a -> vector (r * sin a) (r * cos a))
        (Random.float 0 starSystemOuterRadius)
        (Random.float 0 (turns 1))


randomShip : Int -> Random.Generator Ship
randomShip controllerId =
    Random.map2
        (\position name -> Ship controllerId position (Spawining 0) name)
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

        ControlShip ship ( movement, heading, isFiring ) ->
            model

        RemoveShip ship ->
            model

        Tick dt ->
            model
