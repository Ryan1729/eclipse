module MaterialUpdate exposing (..)

import MaterialModel exposing (MaterialModel)
import MaterialMsg exposing (MaterialMsg(Mdl, U))
import Material
import Update exposing (update)


--This is basically just a passthrough except for what should go to Material.update


materialUpdate : MaterialMsg -> MaterialModel -> ( MaterialModel, Cmd MaterialMsg )
materialUpdate msg materialModel =
    case msg of
        U message ->
            let
                ( model, cmds ) =
                    update message materialModel.model
            in
                ( { materialModel | model = model }, Cmd.map U cmds )

        Mdl msg' ->
            Material.update msg' materialModel
