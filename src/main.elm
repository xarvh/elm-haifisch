
import Collage exposing (..)
import Color exposing (..)
import Element
import Html.App as App
import Time
import Keyboard


import Game.Main as Game
import Game.Player as Player
import Game.Ship as Ship



type alias Model =
    { game : Game.Model
    , currentPlayerId : Game.PlayerId
    }


type Message
    = Noop
    | PlayerInput Player.Message
    | Tick Time.Time


noCmd model =
    (model, Cmd.none)


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        Noop ->
            noCmd model

        PlayerInput playerMessage ->
            noCmd { model | game = Game.update (Game.PlayerMessage model.currentPlayerId playerMessage) model.game }

        Tick time ->
            noCmd { model | game = Game.update Game.Tick model.game }




drawShip player =
    Collage.ngon 5 50
        |> Collage.filled Color.blue
        |> Collage.rotate player.ship.angle
        |> Collage.move (player.ship.position.x - Ship.size/2, player.ship.position.y - Ship.size/2)




view model =
 Element.toHtml
    <| Collage.collage (floor Ship.size) (floor Ship.size)
    <| List.map (snd >> drawShip) model.game.players




shipControls =
    [   { down = Ship.Turn (Just Ship.Clockwise)
        , up = Ship.Turn Nothing
        , keyCodes = [13, 39, 76, 68] -- Enter, Arrow Right, l, d
        }
    ,   { down = Ship.Turn (Just Ship.CounterClockwise)
        , up = Ship.Turn Nothing
        , keyCodes = [37, 72, 65] -- Arrow Left, h, a
        }
    ,   { down = Ship.Thrust True
        , up = Ship.Thrust False
        , keyCodes = [32] -- Spacebar
        }
    ]


keyPressDispatcher what keyCodeMap keyCode =
    case keyCodeMap of
        x :: xs -> if List.member keyCode x.keyCodes then what x else keyPressDispatcher what xs keyCode
        _ -> Noop --let x = Debug.log "keyCode" keyCode in Noop



subscriptions model =
    let
        w component shipMessages =
            (component shipMessages) |> Player.CommandShip |> PlayerInput

    in
        Sub.batch
            [ Time.every (100 * Time.millisecond) Tick
            , Keyboard.ups (keyPressDispatcher (w .up) shipControls)
            , Keyboard.downs (keyPressDispatcher (w .down) shipControls)
            ]


init =
    let
        newGame =
            Game.update Game.AddPlayer Game.init

        currentPlayerId =
            Maybe.withDefault 0 <| List.head newGame.players `Maybe.andThen` (fst >> Just)
    in
        ( Model newGame currentPlayerId, Cmd.none)







main = App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

