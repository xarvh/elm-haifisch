module Game.Ship exposing (..)



import Time



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
maxVelocity = size / Time.second

shipThrust = 0.1 * maxVelocity / Time.second
shipTurnRate = turns 0.2 / Time.second



type alias Model =
    { position : Vector
    , velocity : Vector
    , angle : Float

    -- These indicate what the player WANTS to do, not necessarily what the ship is doing
    , commandThrust : Bool
    , commandTurn : Maybe TurnDirection
    }



type Message
    = Tick Time.Time
    | Turn (Maybe TurnDirection)
    | Thrust Bool



init =
    Model { x = size/2, y = size/2} { x = 0, y = 0 } 0 False Nothing



wrap size value =
   value - (toFloat <| floor <| value / size) * size


updatePositionByComponent deltaTime model component =
    wrap size <| component model.position + component model.velocity * deltaTime


updateVelocityByComponent deltaTime thrust model component =
    clamp (-maxVelocity) maxVelocity <| component model.velocity + component thrust * deltaTime



tick : Time.Time -> Model -> Model
tick dt model =
    let
        pos =
            updatePositionByComponent dt model

        newPosition =
            { x = pos .x, y = pos .y }

        thrust =
            if model.commandThrust
            then { x = shipThrust * cos model.angle, y = shipThrust * sin model.angle }
            else { x = 0, y = 0 }

        vel =
            updateVelocityByComponent dt thrust model

        newVelocity =
            { x = vel .x, y = vel .y }

        newAngle =
            case model.commandTurn of
                Nothing -> model.angle
                Just direction ->
                    wrap (turns 1) <| model.angle + shipTurnRate * dt * case direction of
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
        Tick dt -> tick dt model
        Turn maybeTurn -> { model | commandTurn = maybeTurn }
        Thrust isThrusting -> { model | commandThrust = isThrusting }

