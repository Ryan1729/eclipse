module View exposing (view)

import MaterialModel exposing (MaterialModel)
import Model exposing (Model, Ball, Board, GameState(..), PinId(..), Pin(..), Ball(..), Rack)
import Html exposing (Html, text)
import Html.App
import Html.Attributes
import MaterialMsg exposing (MaterialMsg(Mdl, U))
import Msg exposing (Msg(..))
import Material.Button as Button
import Material.Grid as Grid exposing (Device(..))
import Svg exposing (Svg, svg, rect, path, circle, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


view : MaterialModel -> Html MaterialMsg
view { mdl, model } =
    Html.div []
        [ Button.render Mdl
            [ 0 ]
            mdl
            [ Button.raised
            , Button.ripple
            , Button.onClick (U NewGame)
            ]
            [ text "New Game" ]
        , Grid.grid []
            [ Grid.cell [ Grid.size All 5 ]
                [ renderRack model.rack
                ]
            , Grid.cell [ Grid.size All 6 ]
                [ Html.div [ Html.Attributes.style [ ( "width", boardWidthString ++ "px" ), ( "display", "flex" ), ( "justify-content", "center" ), ( "font-size", (boardWidth / 32 |> toString) ++ "px" ) ] ]
                    [ model
                        |> gameStateToString
                        |> Html.text
                    ]
                , svg
                    [ width boardWidthString
                    , height boardHeightString
                    , viewBox ("0 0 " ++ boardWidthString ++ " " ++ boardHeightString)
                    ]
                    [ Svg.defs []
                        [ Svg.clipPath [ id "ballClip", clipPathUnits "objectBoundingBox" ]
                            [ rect
                                [ x "0"
                                , y "0.125"
                                , width "1"
                                , height "0.75"
                                ]
                                []
                            ]
                        ]
                    , renderBoard model.board
                    ]
                ]
            ]
            |> Html.App.map U
        ]


gameStateToString : Model -> String
gameStateToString model =
    case model.gameState of
        Win ->
            "You won!"

        Loss ->
            "You lost!"

        _ ->
            "CPU: "
                ++ toString (Model.currentScore Red model.board)
                ++ " -  You : "
                ++ toString (Model.currentScore White model.board)


renderBoard : Board -> Svg Msg
renderBoard board =
    stand
        :: renderPins board
        |> g []


standX =
    -boardWidth / 8


standY =
    boardHeight / 3


standW =
    boardWidth


standH =
    boardHeight


stand =
    renderStand standX standY standW standH


renderPins : Board -> List (Svg Msg)
renderPins board =
    List.map (renderPin board) Model.pinIdPossibilities


halfPinWidth =
    12.5


halfPinWidthString =
    toString halfPinWidth


pinHeight =
    100


pinHeightString =
    toString pinHeight


renderPin : Board -> PinId -> Svg Msg
renderPin board pinId =
    let
        ( baseX, baseY ) =
            getPinPosition pinId standX standY standW standH

        topLeftCornerX =
            baseX - halfPinWidth

        topLeftCornerY =
            -- + 3 to cover the corner of the lines
            baseY - pinHeight + 3

        ((Pin bottom middle top) as pin) =
            Model.getPin pinId board

        pinExtraAttributes =
            if Model.pinHasRoom pin then
                [ onClick (Place pinId) ]
            else
                []
    in
        [ rect (pinExtraAttributes ++ [ fill "#888888", x (toString topLeftCornerX), y (toString topLeftCornerY), width "25", height pinHeightString ]) []
        , Svg.circle
            (pinExtraAttributes
                ++ [ fill "#888888"
                   , cx <| toString (baseX)
                   , cy (toString topLeftCornerY)
                   , r halfPinWidthString
                   ]
            )
            []
        , renderBall bottom baseX (baseY - pinHeight / 6)
        , renderBall middle baseX (baseY - pinHeight / 2)
        , renderBall top baseX (baseY - pinHeight * 5 / 6)
        ]
            |> g []


renderStand x y w h =
    Svg.path
        [ fill "#111111"
        , d
            <| "M"
            ++ toString (w * 5 / 12 + x)
            ++ " "
            ++ toString (h * 5 / 48 + y)
            ++ (" Q "
                    ++ toString (w * 5 / 6 + x)
                    ++ " "
                    ++ toString (0 + y)
                    ++ " "
                    ++ toString (w * 25 / 24 + x)
                    ++ " "
                    ++ toString (h * 5 / 48 + y)
               )
            ++ (" T "
                    ++ toString (w * 5 / 6 + x)
                    ++ " "
                    ++ toString (h * 5 / 16 + y)
               )
            ++ (" T "
                    ++ toString (w * 5 / 24 + x)
                    ++ " "
                    ++ toString (h * 5 / 16 + y)
               )
            ++ (" T "
                    ++ toString (w * 5 / 12 + x)
                    ++ " "
                    ++ toString (h * 5 / 48 + y)
               )
        ]
        []
        :: renderLines x y w h
        |> g []


renderLines : Float -> Float -> Float -> Float -> List (Svg Msg)
renderLines x y w h =
    let
        ( rightTopX, rightTopY ) =
            getPinPosition Zero x y w h

        ( rightMiddleX, rightMiddleY ) =
            getPinPosition One x y w h

        ( rightBottomX, rightBottomY ) =
            getPinPosition Two x y w h

        ( leftTopX, leftTopY ) =
            getPinPosition Five x y w h

        ( leftMiddleX, leftMiddleY ) =
            getPinPosition Six x y w h

        ( leftBottomX, leftBottomY ) =
            getPinPosition Seven x y w h
    in
        [ Svg.path
            [ fill "transparent"
            , stroke "#888888"
            , strokeWidth "4"
            , d
                <| "M "
                ++ toString leftTopX
                ++ " "
                ++ toString leftTopY
                ++ (" L "
                        ++ toString rightMiddleX
                        ++ " "
                        ++ toString rightMiddleY
                   )
                ++ (" L "
                        ++ toString leftBottomX
                        ++ " "
                        ++ toString leftBottomY
                   )
                ++ "Z"
            ]
            []
        , Svg.path
            [ fill "transparent"
            , stroke "#888888"
            , strokeWidth "4"
            , d
                <| "M "
                ++ toString rightTopX
                ++ " "
                ++ toString rightTopY
                ++ (" L "
                        ++ toString leftMiddleX
                        ++ " "
                        ++ toString leftMiddleY
                   )
                ++ (" L "
                        ++ toString rightBottomX
                        ++ " "
                        ++ toString rightBottomY
                   )
                ++ "Z"
            ]
            []
        ]


getPinPosition : PinId -> Float -> Float -> Float -> Float -> ( Float, Float )
getPinPosition pinId x y w h =
    case pinId of
        Zero ->
            ( w * 39 / 48 + x, h * 6 / 96 + y )

        One ->
            ( w * 22 / 24 + x, h * 13 / 96 + y )

        Two ->
            ( w * 49 / 48 + x, h * 20 / 96 + y )

        Three ->
            ( w * 7 / 12 + x, h / 6 + y )

        Four ->
            ( w * 11 / 16 + x, h * 23 / 96 + y )

        Five ->
            ( w * 6 / 24 + x, h * 19 / 96 + y )

        Six ->
            ( w * 17 / 48 + x, h * 26 / 96 + y )

        Seven ->
            ( w * 11 / 24 + x, h * 33 / 96 + y )


boardWidth =
    720


boardWidthString =
    toString boardWidth


boardHeight =
    720


boardHeightString =
    toString boardHeight


centerX =
    boardWidth / 2


centerY =
    boardHeight / 2


centerXString =
    toString centerX


centerYString =
    toString centerY


rackWidth =
    250


rackHeight =
    600


rackWidthString =
    toString rackWidth


rackHeightString =
    toString rackHeight


renderRack : Rack -> Svg Msg
renderRack rack =
    svg
        [ width rackWidthString
        , height rackHeightString
        , viewBox ("0 0 " ++ rackWidthString ++ " " ++ rackHeightString)
        ]
        <| [ Svg.rect
                [ x "0"
                , y "0"
                , width rackWidthString
                , height rackHeightString
                , stroke "black"
                , strokeWidth "2"
                , fillOpacity "0"
                ]
                []
           ]
        ++ renderBalls rack


renderBalls : Rack -> List (Svg Msg)
renderBalls rack =
    ([0..(toFloat rack.red - 1)]
        |> List.map
            (\index ->
                renderBall Red (rackWidth / 3) (rackHeight * (12 - index - 0.5) / 12)
            )
    )
        ++ ([0..(toFloat rack.white - 1)]
                |> List.map
                    (\index ->
                        renderBall White (rackWidth * 2 / 3) (rackHeight * (12 - index - 0.5) / 12)
                    )
           )


ballRadius =
    pinHeight / 5


ballRadiusString =
    toString ballRadius


nullSvg =
    Svg.text ""


renderBall ball x y =
    case ball of
        NoBall ->
            nullSvg

        Red ->
            renderActualBall [ fill "#FF4136" ] x y

        White ->
            renderActualBall [ fill "#EEEEEE" ] x y


renderActualBall extraAttributes xPos yPos =
    circle ([ clipPath "url(#ballClip)", cx (toString xPos), cy (toString yPos), r ballRadiusString ] ++ extraAttributes) []
