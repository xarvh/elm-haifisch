module Client exposing (..)


import Keyboard
import Time
import View
import UI


init =
    UI.init

update =
    UI.update

view model =
    View.render model.currentPlayerId model






-- shipControls =
--     [   { down = Ship.Turn (Just Ship.Clockwise)
--         , up = Ship.Turn Nothing
--         , keyCodes = [13, 39, 76, 68] -- Enter, Arrow Right, l, d
--         }
--     ,   { down = Ship.Turn (Just Ship.CounterClockwise)
--         , up = Ship.Turn Nothing
--         , keyCodes = [37, 72, 65] -- Arrow Left, h, a
--         }
--     ,   { down = Ship.Thrust True
--         , up = Ship.Thrust False
--         , keyCodes = [32] -- Spacebar
--         }
--     ]





-- keyPressDispatcher what keyCodeMap keyCode =
--     case keyCodeMap of
--         x :: xs -> if List.member keyCode x.keyCodes then what x else keyPressDispatcher what xs keyCode
--         _ -> Noop --let x = Debug.log "keyCode" keyCode in Noop






subscriptions model =
    Sub.none

--     let
--         key component shipMessages =
--             (component shipMessages) |> Player.CommandShip |> PlayerInput
-- 
--     in
--         Sub.batch
--             [ Time.every granularity Tick
--             , Keyboard.ups <| keyPressDispatcher (key .up) shipControls
--             , Keyboard.downs <| keyPressDispatcher (key .down) shipControls
--             ]
