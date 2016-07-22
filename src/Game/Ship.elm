module Game.Ship exposing (..)


-- TODO: Vector and TurnDirection should be in some generic module...
type alias Vector =
    { x : Float
    , y : Float
    }


type TurnDirection
    = Clockwise
    | CounterClockwise



type alias Model =
    { position : Vector
    , velocity : Vector
    , angle : Float

    -- These indicate what the player WANTS to do, not necessarily what the ship is doing
    , commandThrust : Bool
    , commandTurn : Maybe TurnDirection
    }



type Message
    = Turn (Maybe TurnDirection)
    | Thrust Bool



update : Message -> Model -> Model
update message model =
    case message of
        Turn maybeTurn -> { model | commandTurn = maybeTurn }
        Thrust isThrusting -> { model | commandThrust = isThrusting }

