module Game.Main exposing (..)


import Game.Player as Player



type alias PlayerId = Int



type alias Model =
    { nextPlayerId : PlayerId
    , players : List (PlayerId, Player.Model)
    }




type Message
    = Tick
    | PlayerMessage PlayerId Player.Message




tick model =
    model



update : Message -> Model -> Model
update message model =
    case message of
        Tick -> tick model
        PlayerMessage playerId playerMessage ->
            let
                updatePlayer (id, player) =
                    ( id, if id /= playerId then player else Player.update playerMessage player )
            in
                { model | players = List.map updatePlayer model.players }

