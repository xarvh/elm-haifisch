module Algebra exposing (..)


import Math.Vector2 as V


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
