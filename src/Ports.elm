port module Ports exposing (..)

import Json.Decode


port animationFrameAndGamepads : (Json.Decode.Value -> msg) -> Sub msg


port playSound : String -> Cmd msg
