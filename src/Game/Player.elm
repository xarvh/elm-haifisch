module Game.Player exposing (..)


import Game.Ship as Ship



type alias Model =
    { ship : Ship.Model
    , score : Int
    }



type Message =
    CommandShip Ship.Message



update : Message -> Model -> Model
update message model =
    case message of
        CommandShip shipMessage ->
            { model | ship = Ship.update shipMessage model.ship }

