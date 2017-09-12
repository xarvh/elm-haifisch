module Config.Remap exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Gamepad


type alias Model =
    {}


type Msg
    = Noop



-- Init


init =
    noCmd {}



-- Update


noCmd model =
    ( model, Cmd.none )


update msg model =
    noCmd model



-- View


view model =
    text ""
