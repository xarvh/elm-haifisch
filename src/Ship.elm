module Ship exposing (..)



import Math.Vector2 as V
import Time exposing (Time)




-- Math stuff

type alias Vector =
    V.Vec2


vector =
    V.vec2


vectorToString : Vector -> String
vectorToString v =
    toString (V.getX v) ++ "," ++ toString (V.getY v)


-- Ship stuff


type alias ActiveModel =
    { position : Vector
    , velocity : Vector
    , gunCooldown : Time
    }


type Status
    = Spawining Float
    | Active ActiveModel
    | Exploding Float


type alias Ship =
    { controllerId : Int
    , status : Status
    , name : String
    }
