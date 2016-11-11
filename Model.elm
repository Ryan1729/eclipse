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
    { zero : Pin
    , one : Pin
    , two : Pin
    , three : Pin
    , four : Pin
    , five : Pin
    , six : Pin
    , seven : Pin
    }


initialBoard : Board
initialBoard =
    { zero = emptyPin
    , one = Pin White Red White
    , two = emptyPin
    , three = emptyPin
    , four = emptyPin
    , five = emptyPin
    , six = emptyPin
    , seven = emptyPin
    }


type PinId
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven


pinIdPossibilities =
    [ Zero
    , One
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    ]


getPin pinId board =
    case pinId of
        Zero ->
            board.zero

        One ->
            board.one

        Two ->
            board.two

        Three ->
            board.three

        Four ->
            board.four

        Five ->
            board.five

        Six ->
            board.six

        Seven ->
            board.seven


type Pin
    = Pin Ball Ball Ball


emptyPin =
    Pin NoBall NoBall NoBall


type Ball
    = NoBall
    | Red
    | White


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
