module Common exposing (..)

import Math.Vector2 as V
import Time exposing (Time)


-- TYPES
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



-- ALGEBRA


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
        if ll <= radius * radius then
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
