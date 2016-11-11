module View exposing (view)

import MaterialModel exposing (MaterialModel)
import Model exposing (Model, Piece, Board, GameState(..), PinId(..))
import Html exposing (Html, text)
import Html.App
import Html.Attributes
import MaterialMsg exposing (MaterialMsg(Mdl, U))
import Msg exposing (Msg(..))
import Material.Button as Button
import Material.Grid as Grid exposing (Device(..))
import Svg exposing (Svg, svg, rect, path, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import PieceView


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
                [ PieceView.renderRack model.selected model.rack
                ]
            , Grid.cell [ Grid.size All 6 ]
                [ Html.div [ Html.Attributes.style [ ( "width", boardWidthString ++ "px" ), ( "display", "flex" ), ( "justify-content", "center" ), ( "font-size", (boardWidth / 32 |> toString) ++ "px" ) ] ]
                    [ model.gameState
                        |> gameStateToString
                        |> Html.text
                    ]
                , svg
                    [ width boardWidthString
                    , height boardHeightString
                    , viewBox ("0 0 " ++ boardWidthString ++ " " ++ boardHeightString)
                    ]
                    [ renderBoard model.selected model.board
                    ]
                ]
            ]
            |> Html.App.map U
        ]


gameStateToString : GameState -> String
gameStateToString gameState =
    case gameState of
        Win ->
            "You won!"

        Loss ->
            "You lost!"

        _ ->
            ""


renderBoard : Maybe Piece -> Board -> Svg Msg
renderBoard selected board =
    stand
        :: renderPins selected board
        |> g []


stand =
    renderStand -(boardWidth / 8) (boardHeight / 3) (boardWidth) (boardHeight)


renderPins : Maybe Piece -> Board -> List (Svg Msg)
renderPins selected board =
    [ rect [ fill "#888888", x centerXString, y centerYString, width "25", height "100" ] []
    , Svg.circle [ fill "#888888", cx <| toString (centerX + 12.5), cy centerYString, r "12.5" ] []
    ]


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



-- rect
--     [ x centerXString
--     , y centerYString
--     , width (toString w)
--     , height (toString h)
--     , rx (toString (w * 3 / 10))
--     , ry (toString (h * 3 / 10))
--     , transform <| "rotate(-35, " ++ centerXString ++ " " ++ centerYString ++ ")"
--     ]
--     []


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
