module MaterialModel exposing (..)

import Material
import Model exposing (Model)


type alias MaterialModel =
    { mdl : Material.Model, model : Model }


defaultState =
    { mdl = Material.model, model = Model.defaultModel }
