module Model exposing (..)


type alias Model =
    { board : Board
    , rack : Rack
    , gameState : GameState
    }


defaultModel =
    { board = initialBoard
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
    , one = emptyPin
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


place : Ball -> PinId -> Board -> Board
place ball pinId board =
    case pinId of
        Zero ->
            { board | zero = placeOnPin ball board.zero }

        One ->
            { board | one = placeOnPin ball board.one }

        Two ->
            { board | two = placeOnPin ball board.two }

        Three ->
            { board | three = placeOnPin ball board.three }

        Four ->
            { board | four = placeOnPin ball board.four }

        Five ->
            { board | five = placeOnPin ball board.five }

        Six ->
            { board | six = placeOnPin ball board.six }

        Seven ->
            { board | seven = placeOnPin ball board.seven }


placeOnPin newBall ((Pin bottom middle top) as pin) =
    if bottom == NoBall then
        Pin newBall middle top
    else if middle == NoBall then
        Pin bottom newBall top
    else if top == NoBall then
        Pin bottom middle newBall
    else
        pin


type Pin
    = Pin Ball Ball Ball


emptyPin =
    Pin NoBall NoBall NoBall


pinHasRoom (Pin bottom middle top) =
    bottom == NoBall || middle == NoBall || top == NoBall


type Ball
    = NoBall
    | Red
    | White


type alias Rack =
    { red : Int
    , white : Int
    }


initialRack : Rack
initialRack =
    { red = 12
    , white = 12
    }


removeFromRack : Ball -> Rack -> Rack
removeFromRack ball rack =
    case ball of
        NoBall ->
            rack

        Red ->
            { rack | red = max 0 <| rack.red - 1 }

        White ->
            { rack | white = max 0 <| rack.white - 1 }


getAvailablePinIds : Board -> List PinId
getAvailablePinIds board =
    pinIdPossibilities
        |> List.filter
            (\pinId ->
                board
                    |> getPin pinId
                    |> pinHasRoom
            )


currentScore : Ball -> Board -> Int
currentScore ball board =
    countMultiPinPoints ball board.zero board.one board.two
        + countMultiPinPoints ball board.zero board.three board.six
        + countMultiPinPoints ball board.one board.four board.seven
        + countMultiPinPoints ball board.one board.three board.five
        + countMultiPinPoints ball board.two board.four board.six
        + countMultiPinPoints ball board.five board.six board.seven
        + countSinglePinPoints ball board.zero
        + countSinglePinPoints ball board.one
        + countSinglePinPoints ball board.two
        + countSinglePinPoints ball board.three
        + countSinglePinPoints ball board.four
        + countSinglePinPoints ball board.five
        + countSinglePinPoints ball board.six
        + countSinglePinPoints ball board.seven


countSinglePinPoints ball (Pin bottom middle top) =
    countMatches ball bottom middle top


countMultiPinPoints : Ball -> Pin -> Pin -> Pin -> Int
countMultiPinPoints ball (Pin bottom1 middle1 top1) (Pin bottom2 middle2 top2) (Pin bottom3 middle3 top3) =
    countMatches ball bottom1 bottom2 bottom3
        + countMatches ball middle1 middle2 middle3
        + countMatches ball top1 top2 top3
        + countMatches ball bottom1 middle2 top3
        + countMatches ball top1 middle2 bottom3


countMatches : Ball -> Ball -> Ball -> Ball -> Int
countMatches selectedBallType ball1 ball2 ball3 =
    if selectedBallType == ball1 && ball1 == ball2 && ball2 == ball3 then
        1
    else
        0
