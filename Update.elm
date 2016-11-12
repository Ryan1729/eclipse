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
            if model.rack.white > 0 then
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
            else
                ( model, Cmd.none )


type alias Move =
    ( Ball, PinId )


getMoves : Rack -> Board -> List Move
getMoves rack board =
    List.map ((,) Red)
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

        postRackUpdateModel =
            { postMovementModel | rack = removeFromRack Red postMovementModel.rack }
    in
        if isCPULosingModel postRackUpdateModel then
            { postRackUpdateModel | gameState = Win }
        else if isUserLosingModel postRackUpdateModel then
            { postRackUpdateModel | gameState = Loss }
        else
            postRackUpdateModel


applyMove : Model -> Move -> Model
applyMove model ( ball, pinId ) =
    { model | board = Model.place ball pinId model.board }
