module PieceView exposing (..)

import Svg exposing (Svg, svg, rect, path, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Model exposing (..)
import Msg exposing (Msg(..))


rackWidth =
    250


rackHeight =
    600


rackWidthString =
    toString rackWidth


rackHeightString =
    toString rackHeight


renderRack : Maybe Piece -> Rack -> Svg Msg
renderRack selected rack =
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
        ++ renderPieces selected rack


renderPieces : Maybe Piece -> Rack -> List (Svg Msg)
renderPieces selected rack =
    [ Svg.text "pieces" ]
