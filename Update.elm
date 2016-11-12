module Update exposing (update, getMoves)

import Msg exposing (Msg(..))
import Model exposing (..)
import Extras
import Random.Pcg as Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( Model.defaultModel, Cmd.none )

        Place pinId ->
            if model.gameState == InProgress then
                let
                    newModel =
                        { model
                            | board = Model.place White pinId model.board
                            , rack = Model.removeFromRack White model.rack
                        }
                in
                    if isCPULosingModel newModel then
                        ( { newModel | gameState = Win }, Cmd.none )
                    else if isUserLosingModel newModel then
                        ( { newModel | gameState = Loss }, Cmd.none )
                    else if isTieModel newModel then
                        ( { newModel | gameState = Tie }, Cmd.none )
                    else
                        ( cpuTurn newModel, Cmd.none )
            else
                ( model, Cmd.none )


type alias Move =
    ( Ball, PinId )


getMoves : Ball -> Rack -> Board -> List Move
getMoves ball rack board =
    List.map ((,) ball)
        (Model.getAvailablePinIds board)
        |> Extras.shuffle (Random.initialSeed 42)


isCPULosingModel : Model -> Bool
isCPULosingModel model =
    let
        rack =
            model.rack
    in
        if rack.red <= 0 && rack.white <= 0 then
            currentScore Red model.board <= currentScore White model.board
        else
            False


isUserLosingModel : Model -> Bool
isUserLosingModel model =
    let
        rack =
            model.rack
    in
        if rack.red <= 0 && rack.white <= 0 then
            Model.currentScore White model.board <= Model.currentScore Red model.board
        else
            False


isTieModel : Model -> Bool
isTieModel model =
    let
        rack =
            model.rack
    in
        if rack.red <= 0 && rack.white <= 0 then
            Model.currentScore White model.board == Model.currentScore Red model.board
        else
            False


nextPlayerHasNoWinningMove : Model -> Move -> Bool
nextPlayerHasNoWinningMove model move =
    let
        potentialModel =
            applyMove model move

        potentialFutureMoves =
            getMoves White model.rack potentialModel.board
    in
        case Extras.find (userWinningMove potentialModel) potentialFutureMoves of
            Just _ ->
                False

            Nothing ->
                True


nextPlayerHasNoScoreIncreasingMove : Model -> Move -> Bool
nextPlayerHasNoScoreIncreasingMove model move =
    let
        potentialModel =
            applyMove model move

        potentialFutureMoves =
            getMoves White model.rack potentialModel.board
    in
        case Extras.find (userScoreIncreasingMove potentialModel) potentialFutureMoves of
            Just _ ->
                False

            Nothing ->
                True


userWinningMove : Model -> Move -> Bool
userWinningMove model move =
    applyMove model move
        |> isCPULosingModel


cpuWinningMove : Model -> Move -> Bool
cpuWinningMove model move =
    applyMove model move
        |> isUserLosingModel


cpuScoreIncreasingMove : Model -> Move -> Bool
cpuScoreIncreasingMove =
    isScoreIncreasingMove Red


userScoreIncreasingMove : Model -> Move -> Bool
userScoreIncreasingMove =
    isScoreIncreasingMove White


isScoreIncreasingMove ball model move =
    let
        oldScore =
            Model.currentScore ball model.board

        newScore =
            Model.currentScore ball (applyMove model move).board
    in
        newScore > oldScore


cpuTurn : Model -> Model
cpuTurn model =
    let
        moves : List Move
        moves =
            getMoves Red model.rack model.board

        postMovementModel =
            Extras.find (cpuWinningMove model) moves
                |> Extras.orElseLazy (\() -> Extras.find (cpuScoreIncreasingMove model) moves)
                |> Extras.orElseLazy (\() -> Extras.find (nextPlayerHasNoScoreIncreasingMove model) moves)
                |> Extras.orElseLazy (\() -> Extras.find (nextPlayerHasNoWinningMove model) moves)
                |> Extras.orElseLazy (\() -> Random.step (Random.sample moves) (Random.initialSeed 42) |> fst)
                |> Maybe.map (applyMove model)
                |> Maybe.withDefault model

        postRackUpdateModel =
            { postMovementModel | rack = removeFromRack Red postMovementModel.rack }
    in
        if isCPULosingModel postRackUpdateModel then
            { postRackUpdateModel | gameState = Win }
        else if isUserLosingModel postRackUpdateModel then
            { postRackUpdateModel | gameState = Loss }
        else if isTieModel postRackUpdateModel then
            { postRackUpdateModel | gameState = Tie }
        else
            postRackUpdateModel


applyMove : Model -> Move -> Model
applyMove model ( ball, pinId ) =
    { model | board = Model.place ball pinId model.board }
