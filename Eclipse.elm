module Eclipse exposing (..)

import Html.App exposing (program)
import MaterialModel exposing (MaterialModel, defaultState)
import View exposing (view)
import MaterialMsg exposing (MaterialMsg)
import MaterialUpdate exposing (materialUpdate)


init : ( MaterialModel, Cmd MaterialMsg )
init =
    ( defaultState, Cmd.none )


subscriptions : MaterialModel -> Sub MaterialMsg
subscriptions =
    always Sub.none


main : Program Never
main =
    program
        { init = init
        , update = materialUpdate
        , subscriptions = subscriptions
        , view = view
        }
