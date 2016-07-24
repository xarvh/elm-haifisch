module Game.Player exposing (..)


import Game.Ship as Ship
import Time



type alias Model =
    { ship : Ship.Model
    , score : Int
    }



type Message
    = Tick Time.Time
    | CommandShip Ship.Message


init =
    Model Ship.init 0


update : Message -> Model -> Model
update message model =
    case message of
        Tick dt ->
            { model | ship = Ship.update (Ship.Tick dt) model.ship }

        CommandShip shipMessage ->
            { model | ship = Ship.update shipMessage model.ship }

