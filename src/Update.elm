module Update exposing (update)
import Model exposing(..)
import Ports

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clicked id descr ->
      ( model
      , Ports.askConfirmation id descr
      )
    DoneConfirmation id isConfirmed ->
      ( if isConfirmed
          then
            { model
            | todos =
                model.todos
                |> List.map (\td ->
                    if td.id == id
                    then {td | timeDone = Just model.now}
                    else td
                    )
            }
          else model
      , Cmd.none
      )
    PopulateTodos todos ->
      ( { model
        | todos = todos
        }
      , Cmd.none
      )
    ErrorMessage errMsg ->
      ( { model
        | error = Just errMsg
        }
      , Cmd.none
      )
    Tick now ->
      ( { model
        | now = now}
      , Cmd.none
      )