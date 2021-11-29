module Main exposing (..)

import Browser
import Ports
import Time
import Model exposing(..)
import View
import Update
import HttpCmds

init : Flags -> (Model, Cmd Msg)
init _ =
  ( { todos = []
    , error = Nothing
    , now = Time.millisToPosix 0
    }
  , HttpCmds.fetchTodos
  )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 1000 Tick
    , Ports.receiveConfirmation DoneConfirmation
    ]

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = View.view
    , update = Update.update
    , subscriptions = subscriptions
    }
