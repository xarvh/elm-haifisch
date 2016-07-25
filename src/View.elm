module View exposing (render)



import Game.Main as Game
import Svg exposing (..)



drawShip viewerPlayerId player =
    let
        ship = player.ship

        size = 30
        half = size / 2
    in
        Collage.group
            [ Collage.ngon 3 half
                |> Collage.filled Color.blue
                |> Collage.move (0.6 * half, 0)
            , Collage.ngon 6 half
                |> Collage.filled Color.blue
                |> Collage.move (-0.6 * half, 0)
            ]
                |> Collage.rotate player.ship.angle
                |> Collage.move (player.ship.position.x - Ship.size/2, player.ship.position.y - Ship.size/2)



render : Int -> Game.Model -> Html a
render viewerPlayerId gameModel =
    svg
        [ width "100%"
        , height "100%"
        , viewBox "0 0 1 1"
        ]
        List.concat
            [ List.map (drawShip viewerPlayerId) gameModel.players
            ]

--  Element.toHtml
--     <| Collage.collage (floor Ship.size) (floor Ship.size)
--     <| List.map (snd >> drawShip) model.game.players
