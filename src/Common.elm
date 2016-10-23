module Common exposing (..)

import Algebra exposing (..)
import Time exposing (Time)


-- base length units


shipLength =
    1.0


worldRadius =
    17 * shipLength



-- basic constants


projectileSpeed =
    1 * worldRadius / Time.second



-- TYPES


type alias Coloration =
    ( String, String, String )


type alias Control =
    { thrust : Vector
    , heading : Vector
    , fire : Bool
    }


type alias Player =
    { name : String
    , score : Int
    , coloration : Coloration
    , lastControl : Time
    , control : Maybe Control
    }


type ShipStatus
    = Spawning
    | Active
    | Exploding


type alias Ship =
    { playerName : String
    , position : Vector
    , heading : Float
    , status : ShipStatus
    , name : String
    , reloadTime : Time
    , explodeTime : Time
    , respawnTime : Time
    }


type alias Projectile =
    { id : Int
    , playerName : Int
    , position : Vector
    , heading : Float
    }


type alias Planet =
    { orbitRadius : Float
    , angularSpeed : Float
    , angle : Float
    , surfaceRadius : Float
    , satellites : List Satellite
    }


type alias Satellite =
    { orbitRadius : Float
    , angularSpeed : Float
    , angle : Float
    }



-- Deltas describe generic changes in the game model


type Delta
    = AddProjectile Projectile
    | RemoveProjectile Int
    | AddShip Player
    | DamageShip String
    | RemoveShip String



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
