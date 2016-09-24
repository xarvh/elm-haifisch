module UiMain exposing (..)

import Dict
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
import Svg as S
import Svg.Attributes as SA
import StarSystemUi
import UiCommon as Ui


-- MODEL


type alias Model =
    Ui.UiShared
        { selectedFleetsUi : SelectedFleetsUi.Model
        , starSystemUi : StarSystemUi.Model
        }


init : ( Model, Cmd Msg )
init =
    let
        ( starModel, starCmd ) =
            StarSystemUi.init
    in
        ( { selection = Ui.FleetSelection (Set.fromList [0..4])
          , ctrl = False
          , shift = False
          , selectedFleetsUi = SelectedFleetsUi.init
          , starSystemUi = starModel
          }
        , Cmd.map ToStarSystemUiMsg starCmd
        )



-- COMMANDS FACTORIES


mergeFleets : Game -> Model -> List G.Command
mergeFleets game model =
    let
        compare fleetA fleetB =
            if List.length fleetA.ships < List.length fleetB.ships then
                fleetB
            else
                fleetA

        fold id fleet maybeBest =
            case maybeBest of
                Nothing ->
                    Just fleet

                Just best ->
                    Just <| compare fleet best

        selectedFleets =
            G.selectedFleets (Ui.fleetIds model) game.fleets

        maybeRemainingFleet =
            Dict.foldl fold Nothing selectedFleets

        remainingFleetId =
            Maybe.map .id maybeRemainingFleet
                |> Maybe.withDefault 0

        others =
            selectedFleets
                |> Dict.remove remainingFleetId
                |> Dict.keys
                |> Set.fromList
    in
        if Set.isEmpty others then
            []
        else
            [ G.FleetCommand others (Ui.queueMode model) (G.MergeWith remainingFleetId) ]



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


infoBox : G.Game -> Model -> H.Html Msg
infoBox game model =
    H.div
        [ HA.class "info-box" ]
        [ H.ul [] <|
            List.map
                (\s -> H.li [] [ H.text s ])
                [ "Mouse left: select / marking box"
                , "Mouse right: move / attack (not implemented)"
                , "M: merge selected fleets"
                , "Hold Shift to queue commands"
                , "Space: "
                    ++ (if game.pause then
                            "unpause"
                        else
                            "pause"
                       )
                ]
        ]



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
        [ HA.class "star-system-container full-window" ]
        [ StarSystemUi.view viewerPlayerId game model model.starSystemUi
            |> App.map ToStarSystemUiMsg
        ]


background game =
    let
        ( fill, stroke, strokeWidth ) =
            if game.pause then
                ( "#000030", "#000075", "3px" )
            else
                ( "#000030", "black", "10px" )
    in
        S.svg
            [ SA.class "ui-background full-window"
            ]
            [ S.defs
                []
                [ S.pattern
                    [ SA.id "hex-background"
                    , SA.width "56px"
                    , SA.height "100px"
                    , SA.patternUnits "userSpaceOnUse"
                    , SA.patternTransform "scale(0.5)"
                    ]
                    [ S.rect
                        [ SA.width "100%"
                        , SA.height "100%"
                        , SA.fill fill
                        ]
                        []
                    , S.path
                        [ SA.d "M28 66L0 50L0 16L28 0L56 16L56 50L28 66L28 100"
                        , SA.fill fill
                        , SA.stroke stroke
                        , SA.strokeWidth strokeWidth
                        ]
                        []
                    ]
                ]
            , S.rect
                [ SA.fill "url(#hex-background)"
                , SA.width "100%"
                , SA.height "100%"
                ]
                []
            ]


view : Int -> Game -> Model -> H.Html Msg
view viewerPlayerId game model =
    H.div
        [ HA.class "ui"
        ]
        [ background game
        , starSystemBox viewerPlayerId game model
        , selectionMenu viewerPlayerId game model
        , infoBox game model
        ]



-- SUBS


subscriptions =
    Sub.batch
        [ Keyboard.downs KeyPress
        , Keyboard.ups KeyRelease
        , Sub.map ToStarSystemUiMsg StarSystemUi.subscriptions
        ]
