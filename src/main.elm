
import Collage exposing (..)
import Color exposing (..)
import Element
import Html.App as App
import Time


import Game.Main as Game


type alias Model =
    { position : Int
    }


type Message
    = Noop
    | Tick Time.Time


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        Noop -> (model, Cmd.none)
        Tick time -> ({ model | position = (model.position + 1) % 100 }, Cmd.none)


view model =
 Element.toHtml <| Collage.collage 500 500
    [   Collage.ngon 5 50
        |> Collage.filled Color.blue
        |> Collage.move (0, toFloat model.position)
    ]



subscriptions model =
    Time.every (20 * Time.millisecond) Tick



init =
    ({ position = 0 }, Cmd.none)


main = App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

