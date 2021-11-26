port module Main exposing (main)

import Browser exposing (element)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Html exposing (Html, div, h1, text)
import Http exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Keyboard

import Time
import Time.Format
import Maybe.Extra as Maybe
import SHA
import Binary

type Id = Id String
type alias Todo =
  { id : Id
  , description : String
  , timeCreated : Time.Posix
  , timeDone : Maybe Time.Posix
  }

mkTodo : String -> Time.Posix -> Todo
mkTodo description timeCreated =
  { id =
      (description ++ (String.fromInt (Time.posixToMillis timeCreated)))
      |> Binary.fromStringAsUtf8
      |> SHA.sha512
      |> Binary.toHex
      |> Id
  , description = description
  , timeCreated = timeCreated
  , timeDone = Nothing
  }

type alias Model =
  { currentTime : Time.Posix
  , todos : List Todo
  , newTodoInput : String
  , error : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { currentTime = Time.millisToPosix 0
    , todos = []
    , newTodoInput = ""
    , error = Nothing
    }
  , fetchTodos
  )


type Msg
  = TodoItemClicked Id String
  | NewTodoInputChanged String
  | AddNewTodo
  | PopulateTodos (List Todo)
  | ErrorMessage String
  | KeyUp Keyboard.RawKey
  | Tick  Time.Posix
  | Confirmation Id Bool
  | ServerResponseAddTodo Id (Result String ())

view : Model -> Html Msg
view model =
  let
    timeZone = Time.customZone 120 []
    timeFormat t =
      Time.posixToMillis t
      |> Time.Format.format timeZone "Day Month Year at Hour:Minute:Second"

    isAddDisabled =
      isTodoAlreadyAdded model.todos model.newTodoInput
      || String.isEmpty model.newTodoInput


    newTodoArea : Html Msg
    newTodoArea =
      Grid.container
        []
        [ Grid.row []
            [ Grid.col []
                [ Input.text
                    [ Input.value model.newTodoInput
                    , Input.onInput NewTodoInputChanged
                    ]
                ]
            , Grid.col []
                [ text ("Now: " ++ timeFormat model.currentTime) ]
            , Grid.col []
                [ Button.button
                    [ Button.onClick AddNewTodo
                    , Button.disabled isAddDisabled
                    , Button.success
                    ]
                    [ text "Add"]
                ]
            ]
        ]
    todoListItem : Todo -> Table.Row Msg
    todoListItem todo =
      let
        itemClass =
          todo.timeDone
          |> Maybe.map (always "done")
          |> Maybe.withDefault "not-done"

      in
        Table.tr
          [ Table.rowAttr (onClick (TodoItemClicked todo.id todo.description))
          , Table.rowAttr (class itemClass)
          ]
          [ Table.td [] [text todo.description]
          , Table.td [] [text (timeFormat todo.timeCreated)]
          , Table.td [] [text (todo.timeDone |> Maybe.map timeFormat |> Maybe.withDefault "in-progress")]
          ]

    todoList : List Todo -> Html Msg
    todoList todos =
      Table.table
      { options = [ Table.striped, Table.hover ]
      , thead =  Table.simpleThead
          [ Table.th [] [ text "Description" ]
          , Table.th [] [ text "Created"]
          , Table.th [] [ text "Done" ]
          ]
      , tbody =
          Table.tbody [] (model.todos |> List.map todoListItem)
      }

    errorInfo : Html Msg
    errorInfo =
      model.error
      |> Maybe.map (\err -> div [class "error"] [ text err])
      |> Maybe.withDefault ( div [] [])
  in
    Grid.container []
      [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
      , Grid.row []
        [ Grid.col []
          [
            div []
            [ h1 [] [text "TODO List"]
            , todoList model.todos
            , newTodoArea
            , errorInfo
            ]
          ]
        ]
      ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    toggleDone : Id -> List Todo -> List Todo
    toggleDone id todos =
      todos
      |> List.map ( \t ->
          if t.id == id
            then
              { t
              | timeDone =
                  case t.timeDone of
                    Just _ -> Nothing
                    Nothing -> Just model.currentTime
              }
            else t
        )

    addNewTodo () =
      let todo = mkTodo model.newTodoInput model.currentTime
      in
        ( { model
          | newTodoInput = ""
          , todos =
              if String.isEmpty (String.trim model.newTodoInput)
                then model.todos
                else model.todos ++ [ todo ]
          , error = Nothing
          }
        , addTodo todo
        )
  in
    case msg of
      Confirmation todoId isConfirmed ->
        ( if isConfirmed
            then { model | todos = toggleDone todoId model.todos, error = Nothing}
            else model
        , Cmd.none
        )

      TodoItemClicked (Id todoIdAsStr) description ->
        ( model
        , askConfirmation (todoIdAsStr, description))

      NewTodoInputChanged chg ->
        ( { model | newTodoInput = chg, error = Nothing}
        , Cmd.none
        )
      KeyUp key ->
        let
          isEnter = Keyboard.anyKeyUpper key
            |> Maybe.map (\k -> k == Keyboard.Enter)
            |> Maybe.withDefault False
          todoExists = (not (isTodoAlreadyAdded model.todos model.newTodoInput))
        in
          if isEnter && todoExists
            then addNewTodo ()
            else (model, Cmd.none)

      AddNewTodo -> addNewTodo ()

      PopulateTodos todos ->
        ( { model | todos = todos, error = Nothing }
        , Cmd.none
        )

      ErrorMessage errMsg ->
        ({model | error = Just ("Error: " ++ errMsg)}
        , Cmd.none)

      Tick time ->
        ( { model | currentTime = time}
        , Cmd.none)

      ServerResponseAddTodo id (Ok ()) ->
        ( model
        , Cmd.none
        )
      ServerResponseAddTodo id (Err errorMessage) ->
        ( { model
          | error = Just errorMessage
          , todos = model.todos |> List.filter (\t -> t.id /= id)
          }
        , Cmd.none
        )

isTodoAlreadyAdded : List Todo -> String -> Bool
isTodoAlreadyAdded todos description =
  todos
  |> List.filter (\t -> t.description == description)
  |> List.isEmpty
  |> not

backendUrl = "/api/tasks"

fetchTodos : Cmd Msg
fetchTodos =
  let
    handleDecoding : Result Error (List Todo) -> Msg
    handleDecoding result = case result of
      Ok todos -> PopulateTodos todos
      Err _ -> ErrorMessage "Failed to get todos"
  in
    Http.get
        { url = backendUrl
        , expect = expectJson handleDecoding (Decode.list todoDecoder)
        }


decodeTime : Decode.Decoder Time.Posix
decodeTime = Decode.int |> Decode.map Time.millisToPosix
decodeId = Decode.string |> Decode.map Id

todoDecoder : Decode.Decoder Todo
todoDecoder =
  Decode.map4
    Todo
    ( Decode.field "id" decodeId)
    ( Decode.field "descr" Decode.string)
    ( Decode.field "timeCreated" decodeTime)
    ( Decode.field "timeDone" (Decode.maybe decodeTime))

encodeTodo : Todo -> Encode.Value
encodeTodo todo =
  let
    (Id idStr) = todo.id
    timeDoneJson =
      todo.timeDone
      |> Maybe.map (\t -> Time.posixToMillis t |> Encode.int)
      |> Maybe.withDefault Encode.null
  in
    Encode.object
      [ ("id", Encode.string idStr)
      , ("descr", Encode.string todo.description)
      , ("timeCreated", Encode.int (Time.posixToMillis todo.timeCreated))
      , ("timeDone", timeDoneJson)
      ]
addTodo : Todo -> Cmd Msg
addTodo todo =
  let
    errorToString : Http.Error -> String
    errorToString error =
        case error of
            BadUrl url -> "The URL " ++ url ++ " was invalid"
            Timeout -> "Unable to reach the server, try again"
            NetworkError ->  "Unable to reach the server, check your network connection"
            BadStatus status -> "Error: " ++ (String.fromInt status)
            BadBody errorMessage -> errorMessage

    handleResponse : Result Http.Error () -> Msg
    handleResponse response =
      response
      |> Result.mapError errorToString
      |> ServerResponseAddTodo todo.id

  in
    Http.post
      { url = backendUrl
      , expect = expectWhatever handleResponse
      , body = Http.jsonBody (encodeTodo todo)
      }

port askConfirmation : (String, String) -> Cmd msg
port receiveConfirmation : ((String, Bool) -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 Tick
    , Keyboard.ups KeyUp
    , receiveConfirmation (\(id, confirmation) -> Confirmation (Id id) confirmation)
    ]

main : Program () Model Msg
main = element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
