module Bots exposing (..)

import Common exposing (Id, InputState)
import Game
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Random


type State
    = StateFullRandomBot FullRandomBot
    | StateStaticBot StaticBot



-- Static Bot


type alias StaticBot =
    {}


staticBot : Game.Model -> State
staticBot game =
    StateStaticBot {}


staticBotThink : Game.Model -> StaticBot -> ( State, InputState )
staticBotThink game model =
    ( StateStaticBot model
    , { finalAim = vec2 0 0
      , fire = True
      , move = vec2 0 0
      }
    )



-- Full Random Bot


type alias FullRandomBot =
    { seed : Random.Seed
    }


fullRandomBot : Game.Model -> State
fullRandomBot game =
    StateFullRandomBot { seed = game.seed }


fullRandomBotThink : Game.Model -> FullRandomBot -> ( State, InputState )
fullRandomBotThink game model =
    let
        vec2Generator =
            Random.map2 vec2 (Random.float -1 1) (Random.float -1 1)

        inputStateGenerator =
            Random.map3 InputState vec2Generator Random.bool vec2Generator

        ( inputState, seed ) =
            Random.step inputStateGenerator model.seed
    in
        ( StateFullRandomBot { seed = seed }, inputState )



-- generics


type alias Model =
    { id : Id
    , state : State
    }


addBot : State -> List Model -> List Model
addBot state bots =
    let
        id =
            bots
                |> List.map .id
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1

        bot =
            { id = id
            , state = state
            }
    in
        bot :: bots


wrapModel : Model -> ( State, InputState ) -> ( Model, InputState )
wrapModel model ( botState, inputState ) =
    ( { model | state = botState }, inputState )


think : Game.Model -> Model -> ( Model, InputState )
think game model =
    wrapModel model <|
        case model.state of
            StateStaticBot s ->
                staticBotThink game s

            StateFullRandomBot s ->
                fullRandomBotThink game s
