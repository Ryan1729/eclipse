module MaterialMsg exposing (..)

import Material
import Msg exposing (Msg)


type MaterialMsg
    = Mdl (Material.Msg MaterialMsg)
    | U Msg
