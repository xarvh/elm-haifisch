module UiMain exposing (..)

import GameCommon as G
    exposing
        ( Game
        , Id
        , Command
        )
import Html as H
import Html.App as App
import Html.Attributes as HA
import Html.Events
import Keyboard
import SelectedFleetsUi
import Set
import StarSystemUi
import UiCommon as Ui


-- import UIView
-- MODEL


type alias Model =
    Ui.UiShared
        { selectedFleetsUi : SelectedFleetsUi.Model
        , starSystemUi : StarSystemUi.Model
        }


init =
    { selection = Ui.FleetSelection (Set.fromList [0..99])
    , ctrl = False
    , shift = False
    , selectedFleetsUi = SelectedFleetsUi.init
    , starSystemUi = StarSystemUi.init
    }



-- SELECTION
-- select selectionType selectedIds model =
--     { model
--     | selection = Ui.FleetSelection selectedIds
--     , selectionBox = Nothing
--     }
-- COMMANDS FACTORIES


mergeFleets : Game -> Model -> List G.Command
mergeFleets game model =
    case G.selectedFleets (Ui.fleetIds model) game.fleets |> List.map .id of
        first :: others ->
            [ G.FleetCommand others (Ui.queueMode model) (G.MergeWith first) ]

        [] ->
            []



-- KEYBOARD


noCmd model =
    ( model, [] )


manageKeys game status keyCode model =
    case keyCode of
        16 ->
            noCmd { model | shift = status }

        17 ->
            noCmd { model | ctrl = status }

        -- space bar
        32 ->
            ( model
            , if status then
                [ G.TogglePause ]
              else
                []
            )

        -- M
        77 ->
            ( model
            , if status then
                mergeFleets game model
              else
                []
            )

        _ ->
            let
                a =
                    Debug.log "key" keyCode
            in
                noCmd model



-- UPDATE


type Msg
    = KeyPress Keyboard.KeyCode
    | KeyRelease Keyboard.KeyCode
    | ToStarSystemUiMsg StarSystemUi.Msg
    | ToSelectedFleetUiMsg SelectedFleetsUi.Msg


update : Msg -> Game -> Model -> ( Model, List Command )
update msg game model =
    case msg of
        KeyPress key ->
            manageKeys game True key model

        KeyRelease key ->
            manageKeys game False key model

        ToStarSystemUiMsg nestedMsg ->
            let
                ( newStarSystemUi, newSelection, cmds ) =
                    StarSystemUi.update nestedMsg game model model.starSystemUi
            in
                ( { model | starSystemUi = newStarSystemUi, selection = newSelection }, cmds )

        ToSelectedFleetUiMsg nestedMsg ->
            let
                ( newSelectedFleetsUi, cmds ) =
                    SelectedFleetsUi.update nestedMsg model.selectedFleetsUi
            in
                ( { model | selectedFleetsUi = newSelectedFleetsUi }, cmds )



-- NOTIFICATIONS


updateForNotification : G.Notification -> Model -> Model
updateForNotification notification model =
    case notification of
        G.FleetHasSplit originalFleetId newFleetId ->
            { model | selection = Ui.FleetSelection <| Set.insert newFleetId (Ui.fleetIds model) }



-- VIEW
-- This gives info on whatever is selected


selectionMenu : Id -> G.Game -> Model -> H.Html Msg
selectionMenu viewerPlayerId game model =
    case model.selection of
        Ui.FleetSelection ids ->
            SelectedFleetsUi.view viewerPlayerId (G.selectedFleets ids game.fleets) model.selectedFleetsUi
                |> App.map ToSelectedFleetUiMsg

        Ui.NoSelection ->
            H.text ""


starSystemBox viewerPlayerId game model =
    H.div
        [ HA.class "star-system-container" ]
        [ StarSystemUi.view viewerPlayerId game model model.starSystemUi
            |> App.map ToStarSystemUiMsg
        ]


view : Int -> Game -> Model -> H.Html Msg
view viewerPlayerId game model =
    H.div
        [ HA.style
            [ ( "width", "100vw" )
            , ( "height", "100vh" )
            ]
        ]
        [ starSystemBox viewerPlayerId game model
        , selectionMenu viewerPlayerId game model
        ]



-- SUBS


subscriptions =
    Sub.batch
        [ Keyboard.downs KeyPress
        , Keyboard.ups KeyRelease
        ]
