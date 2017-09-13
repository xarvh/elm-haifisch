module Bots exposing (..)

import Common exposing (Id, InputState)
import Game
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


type alias Model =
    { id : Id
    }


init : Model
init =
    { id = 1
    }


think : Game.Model -> Model -> ( Model, InputState )
think game model =
    ( model
    , { finalAim = vec2 0 0
      , fire = True
      , move = vec2 0 0
      }
    )
