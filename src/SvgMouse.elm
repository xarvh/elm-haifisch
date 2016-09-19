module SvgMouse exposing (transform)

import Native.SvgMouse


transform : String -> Int -> Int -> ( Float, Float )
transform selector clientX clientY =
    let
        { x, y } =
            Native.SvgMouse.transform selector { x = clientX, y = clientY }
    in
        ( x, y )
