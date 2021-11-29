module HttpCmds exposing (..)
import Http
import Model exposing (..)
import Serialization
backendUrl = "/api/tasks"

fetchTodos : Cmd Msg
fetchTodos =
  let
    handleDecoding : Result Http.Error (List Todo) -> Msg
    handleDecoding result = case result of
      Ok todos -> PopulateTodos todos
      Err _ -> ErrorMessage "Failed to get todos"
  in
    Http.get
        { url = backendUrl
        , expect =
            Http.expectJson
              handleDecoding
              Serialization.todoListDecoder
        }
