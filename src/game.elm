
type alias Connection =
    String


type TurnDirection
    = Clockwise
    | CounterClockwise


type alias Vector =
    { x : Float
    , y : Float
    }



type alias Ship =
    { position : Vector
    , velocity : Vector
    , angle : Float

    -- These indicate what the player WANTS to do, not necessarily what the ship is doing
    , commandThrust : Bool
    , commandTurn : Maybe TurnDirection
    }



type alias Player =
    { connection : Connection
    , ship : Ship
    , score : Int
    }



type alias Model =
    { players : Player
    }



type Action
    = Thrust Bool
    | Turn TurnDirection



type Message
    = Tick
    | PlayerAction Player Action





tick model =
    model



playerAct player action model =
    model




update : Message -> Model -> Model
update message model =
    case message of
        Tick -> tick model
        PlayerAction player action -> playerAct player action model






