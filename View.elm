module View exposing (view)

import MaterialModel exposing (MaterialModel)
import Model exposing (Model, Piece, Board, GameState(..))
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


stand =
    renderStand (boardWidth * 3 / 4) (boardHeight * 5 / 8)


renderStand w h =
    Svg.path
        [ d
            <| "M"
            ++ toString (w * 3 / 8)
            ++ " "
            ++ toString (h / 3)
            ++ (" Q "
                    ++ toString (w * 5 / 8)
                    ++ " "
                    ++ toString (h / 6)
                    ++ " "
                    ++ toString (w * 3 / 4)
                    ++ " "
                    ++ toString (h / 3)
               )
            ++ (" T "
                    ++ toString (w * 5 / 8)
                    ++ " "
                    ++ toString (h * 2 / 3)
               )
            ++ (" T "
                    ++ toString (w / 4)
                    ++ " "
                    ++ toString (h * 2 / 3)
               )
            ++ (" T "
                    ++ toString (w * 3 / 8)
                    ++ " "
                    ++ toString (h / 3)
               )
        ]
        []



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
