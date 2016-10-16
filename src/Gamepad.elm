module Gamepad exposing (Gamepad, animationFrameAndGamepads)

import Json.Decode as D exposing ((:=))
import Ports
import Time exposing (Time)


type alias Gamepad =
    { index : Int
    , axes : List Float
    , buttons : List ( Bool, Float )
    }


decodeButton =
    D.tuple2
        (,)
        D.bool
        D.float


decodeGamepad =
    D.object3
        Gamepad
        ("index" := D.int)
        ("axes" := D.list D.float)
        ("buttons" := D.list decodeButton)


decodeAnimationAndGamepads =
    D.object2
        (,)
        ("dt" := D.float)
        ("gamepads" := D.list decodeGamepad)


valueToAnimationAndGamepads : D.Value -> ( Time, List Gamepad )
valueToAnimationAndGamepads value =
    value
        |> D.decodeValue decodeAnimationAndGamepads
        |> Result.withDefault ( 0, [] )



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
