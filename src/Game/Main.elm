module Game.Main exposing (..)



import Game.Player as Player
import Time



type alias PlayerId = Int



type alias Model =
    { nextPlayerId : PlayerId
    , players : List (PlayerId, Player.Model)
    }




type Message
    = Tick Time.Time
    | AddPlayer
    | PlayerMessage PlayerId Player.Message



init =
    Model 1 []


tick : Time.Time -> Model -> Model
tick dt model =
    let
        mapPlayer (id, player) =
            (id, Player.update (Player.Tick dt) player)

        newPlayers =
            List.map mapPlayer model.players
    in
        { model | players = newPlayers }



update : Message -> Model -> Model
update message model =
    case message of
        Tick dt ->
            tick dt model

        AddPlayer ->
            { model
            | players = (model.nextPlayerId, Player.init) :: model.players
            , nextPlayerId = model.nextPlayerId + 1
            }


        PlayerMessage playerId playerMessage ->
            let
                updatePlayer (id, player) =
                    ( id, if id /= playerId then player else Player.update playerMessage player )
            in
                { model | players = List.map updatePlayer model.players }

