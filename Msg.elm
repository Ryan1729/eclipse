module Msg exposing (..)

import Model exposing (..)


type Msg
    = NewGame
    | Place BoardId
    | Select Piece
