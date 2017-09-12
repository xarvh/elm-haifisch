port module MousePort exposing (button)


port mouseButton : (( Int, Bool ) -> msg) -> Sub msg


button =
    mouseButton
