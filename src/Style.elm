module Style exposing (..)

import StyledHtml.Css as Css exposing (makeClass)


ui =
    makeClass "ui"
        [ "background: black;" ]
        []


starSystemContainer =
    makeClass "starSystemContainer"
        []
        []


splashContainer =
    makeClass "splashContainer"
        [ "position: absolute" ]
        []


fullWindow =
    makeClass "fullWindow"
        [ "width: 100vw"
        , "height: 100vh"
        , "overflow: hidden"
        , "position: absolute"
        ]
        []


splash =
    makeClass "splash"
        [ "position: relative"
        , "top: 20%"
        , "left: 50%"
        , "transform: translate(-50%, -50%)"
        , "border: 2px solid #eee"
        , "background-color: rgba(255, 255, 255, 0.3)"
        , "width: 400px"
        , "color: #ff8d00"
        , "text-align: center"
        ]
        [ Css.selector " p"
            [ "margin: 40px 90px" ]
            []
        ]


name =
    makeClass "name" [] []


score =
    makeClass "score" [] []


scoreboard =
    makeClass "scoreboard"
        []
        [ Css.selector " li"
            [ "list-style: none"
            , "border: 2px solid #eee"
            , "font-weight: bold"
            , "padding: 5px 10px"
            , "margin-bottom: 10px"
            , "text-align: right"
            , "background-color: rgba(255, 255, 255, 0.3)"
            ]
            []
        , Css.andClass name
            [ "font-style: italic" ]
            []
        , Css.andClass score
            [ "font-size: 40px"
              --, "line-height: 0"
            , "margin-top: 0"
            , "margin-bottom: 0"
            ]
            []
        ]


scoreboardContainer =
    makeClass "scoreboardContainer"
        [ "position: absolute" ]
        []
