port module Ports exposing (askConfirmation, receiveConfirmation)
import Model exposing(..)

port askConfirmationPort : (String, String) -> Cmd msg
port receiveConfirmationPort : ((String, Bool) -> msg) -> Sub msg

askConfirmation : Id -> String -> Cmd msg
askConfirmation (Id idStr) description = askConfirmationPort (idStr, description)

receiveConfirmation : (Id -> Bool -> msg) -> Sub msg
receiveConfirmation msgCtor =
  receiveConfirmationPort
    (\(idStr, isConfirmed) -> msgCtor (Id idStr) isConfirmed)