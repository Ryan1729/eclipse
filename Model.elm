module Model exposing (..)


type alias Model =
    { board : Board
    , selected : Maybe Piece
    , rack : Rack
    , gameState : GameState
    }


defaultModel =
    { board = initialBoard
    , selected = Nothing
    , rack = initialRack
    , gameState = InProgress
    }


type GameState
    = InProgress
    | Win
    | Loss


type alias Board =
    {}


initialBoard : Board
initialBoard =
    {}


type Piece
    = Piece


type alias Rack =
    {}


initialRack : Rack
initialRack =
    {}


removeFromRack : Piece -> Rack -> Rack
removeFromRack piece rack =
    rack


type BoardId
    = BoardId


place : Piece -> BoardId -> Board -> Board
place piece boardId board =
    board


getAvailableBoardIds : Board -> List BoardId
getAvailableBoardIds board =
    []


getAvailablePieces : Rack -> List Piece
getAvailablePieces rack =
    []
