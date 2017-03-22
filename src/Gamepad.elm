module Gamepad exposing (Gamepad, animationFrameAndGamepads)

import Json.Decode as D
import Ports
import Time exposing (Time)


type alias Gamepad =
    { index : Int
    , axes : List Float
    , buttons : List ( Bool, Float )
    }


decodeButton =
    D.map2
        (,)
        (D.index 0 D.bool)
        (D.index 1 D.float)


decodeGamepad =
    D.map3
        Gamepad
        (D.field "index" D.int)
        (D.field "axes" <| D.list D.float)
        (D.field "buttons" <| D.list decodeButton)


decodeAnimationAndGamepads =
    D.map2
        (,)
        (D.field "dt" D.float)
        (D.field "gamepads" <| D.list decodeGamepad)


valueToAnimationAndGamepads : D.Value -> ( Time, List Gamepad )
valueToAnimationAndGamepads value =
    case D.decodeValue decodeAnimationAndGamepads value of
        Ok ok ->
            ok

        Err error ->
            Debug.crash error



-- SUBSCRIPTION


{-|
    Replaces [AnimationFrame.diffs](http://package.elm-lang.org/packages/elm-lang/animation-frame/latest).

    Requests the browser's animationFrame AND gamepad status at the same time.

    There are no events defined for gamepad signals so gamepads must be polled, and the best time when to
    do this is
    ["immediately before the animation callbacks are executed"](https://w3c.github.io/gamepad/#usage-examples).
-}
animationFrameAndGamepads : (( Time, List Gamepad ) -> msg) -> Sub msg
animationFrameAndGamepads tagger =
    Ports.animationFrameAndGamepads (valueToAnimationAndGamepads >> tagger)
