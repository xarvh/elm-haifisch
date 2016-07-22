module Game.Player exposing (..)


import Game.Ship as Ship



type alias Model =
    { ship : Ship.Model
    , score : Int
    }



type Message
    = Tick
    | CommandShip Ship.Message


init =
    Model Ship.init 0


update : Message -> Model -> Model
update message model =
    case message of
        Tick ->
            { model | ship = Ship.update Ship.Tick model.ship }

        CommandShip shipMessage ->
            { model | ship = Ship.update shipMessage model.ship }

