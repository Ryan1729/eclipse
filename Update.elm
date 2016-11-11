module Update exposing (update)

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
                else
                    ( cpuTurn newModel, Cmd.none )


type alias Move =
    ( Ball, PinId )


getMoves : Rack -> Board -> List Move
getMoves rack board =
    List.map2 (,)
        (Model.getAvailableBalls rack)
        (Model.getAvailablePinIds board)
        |> Extras.shuffle (Random.initialSeed 42)


isCPULosingModel : Model -> Bool
isCPULosingModel model =
    False


isUserLosingModel : Model -> Bool
isUserLosingModel model =
    False


nextPlayerHasNoWinningMove : Model -> Move -> Bool
nextPlayerHasNoWinningMove model move =
    let
        potentialModel =
            applyMove model move

        potentialFutureMoves =
            getMoves model.rack potentialModel.board
    in
        case Extras.find (userWinningMove potentialModel) potentialFutureMoves of
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


cpuTurn : Model -> Model
cpuTurn model =
    let
        moves : List Move
        moves =
            getMoves model.rack model.board

        postMovementModel =
            Extras.find (cpuWinningMove model) moves
                |> Extras.orElseLazy (\() -> Extras.find (nextPlayerHasNoWinningMove model) moves)
                |> Extras.orElseLazy (\() -> Random.step (Random.sample moves) (Random.initialSeed 42) |> fst)
                |> Maybe.map (applyMove model)
                |> Maybe.withDefault model
    in
        if isCPULosingModel postMovementModel then
            { postMovementModel | gameState = Win }
        else if isUserLosingModel postMovementModel then
            { postMovementModel | gameState = Loss }
        else
            postMovementModel


applyMove : Model -> Move -> Model
applyMove model ( ball, pinId ) =
    { model | board = Model.place ball pinId model.board }
