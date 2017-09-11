module Common exposing (..)

import ColorPattern exposing (ColorPattern)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)


type alias Id =
    Int



-- shipLength is used as unit measure


shipLength : Float
shipLength =
    1.0


worldRadius =
    17 * shipLength



-- TYPES


type alias Player =
    { id : Id
    , score : Int
    , colorPattern : ColorPattern
    }


type alias InputState =
    { finalAim : Vec2
    , fire : Bool
    , move : Vec2
    }



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
    { playerId : Int
    , position : Vec2
    , heading : Float
    , status : Status
    , name : String
    , reloadTime : Time
    , explodeTime : Time
    , respawnTime : Time
    }


type alias Projectile =
    { playerId : Int
    , position : Vec2
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



-- TODO: move this in an 'Algebra' module?
-- ALGEBRA


v0 =
    vec2 0 0


vectorToString : Vec2 -> String
vectorToString v =
    toString (Vec2.getX v) ++ "," ++ toString (Vec2.getY v)


clampToRadius : Float -> Vec2 -> Vec2
clampToRadius radius v =
    let
        ll =
            Vec2.lengthSquared v
    in
        if ll <= radius * radius then
            v
        else
            Vec2.scale (radius / sqrt ll) v


vectorToAngle : Vec2 -> Float
vectorToAngle v =
    let
        ( x, y ) =
            Vec2.toTuple v
    in
        atan2 y x


angleToVector : Float -> Vec2
angleToVector a =
    vec2 (cos a) (sin a)


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


rightHandNormal : Vec2 -> Vec2
rightHandNormal v =
    let
        ( x, y ) =
            Vec2.toTuple v
    in
        vec2 -y x


rotateVector : Float -> Vec2 -> Vec2
rotateVector angle v =
    let
        ( x, y ) =
            Vec2.toTuple v

        sinA =
            sin angle

        cosA =
            cos angle
    in
        vec2
            (x * cosA - y * sinA)
            (x * sinA + y * cosA)
