module Game.Ship exposing (..)


-- TODO: Vector and TurnDirection should be in some generic module...
type alias Vector =
    { x : Float
    , y : Float
    }


type TurnDirection
    = Clockwise
    | CounterClockwise



-- TODO: this should also go somewhere else, possibly the game model
size = 600.0
maxVelocity = size / 10

shipThrust = maxVelocity / 10
shipTurnRate = turns 1 / 40




type alias Model =
    { position : Vector
    , velocity : Vector
    , angle : Float

    -- These indicate what the player WANTS to do, not necessarily what the ship is doing
    , commandThrust : Bool
    , commandTurn : Maybe TurnDirection
    }



type Message
    = Tick
    | Turn (Maybe TurnDirection)
    | Thrust Bool




init =
    Model { x = 0, y = 0} { x = 0, y = 0 } 0 False Nothing



wrap size value =
   value - (toFloat <| floor <| value / size) * size


pos model component =
    wrap size <| component model.position + component model.velocity


vel model thrust component =
    clamp (-maxVelocity) maxVelocity <| component model.velocity + component thrust



tick : Model -> Model
tick model =
    let
        newPosition =
            { x = pos model .x, y = pos model .y }

        thrust =
            if model.commandThrust
            then { x = shipThrust * cos model.angle, y = shipThrust * sin model.angle }
            else { x = 0, y = 0 }

        newVelocity =
            { x = vel model thrust .x, y = vel model thrust .y }

        newAngle =
            case model.commandTurn of
                Nothing -> model.angle
                Just direction ->
                    wrap (turns 1) <| model.angle + shipTurnRate * case direction of
                        Clockwise -> -1
                        CounterClockwise -> 1
    in
        { model
        | position = newPosition
        , velocity = newVelocity
        , angle = newAngle
        }



update : Message -> Model -> Model
update message model =
    case message of
        Tick -> tick model
        Turn maybeTurn -> { model | commandTurn = maybeTurn }
        Thrust isThrusting -> { model | commandThrust = isThrusting }

