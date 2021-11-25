module Main exposing (main)

import Browser exposing (element)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Html exposing (Html, div, h1, text)
import Http exposing (..)
import Json.Decode as Decode

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Keyboard

import Time
import Time.Format
import Maybe.Extra as Maybe

type alias Task =
  { descr : String
  , timeCreated : Time.Posix
  , timeDone : Maybe Time.Posix
  }

type alias Model =
  { currentTime : Time.Posix
  , tasks : List Task
  , newTaskInput : String
  , error : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { currentTime = Time.millisToPosix 0
    , tasks = []
    , newTaskInput = ""
    , error = Nothing
    }
  , fetchTasks
  )


type Msg
  = TaskClicked String
  | NewTaskInputChanged String
  | AddNewTask
  | PopulateTasks (List Task)
  | ErrorMessage String
  | KeyUp Keyboard.RawKey
  | Tick  Time.Posix

view : Model -> Html Msg
view model =
  let
    isAddDisabled =
      isTaskAlreadyAdded model.tasks model.newTaskInput
      || String.isEmpty model.newTaskInput


    newTaskArea : Html Msg
    newTaskArea =
      Grid.container
        []
        [ Grid.row []
            [ Grid.col []
                [ Input.text
                    [ Input.value model.newTaskInput
                    , Input.onInput NewTaskInputChanged
                    ]
                ]
            , Grid.col []
                [ Button.button
                    [ Button.onClick AddNewTask
                    , Button.disabled isAddDisabled
                    , Button.success
                    ]
                    [ text "Add"]
                ]
            ]
        ]
    taskListItem : Task -> Html Msg
    taskListItem task =
      let
        itemClass =
          task.timeDone
          |> Maybe.map (always "done")
          |> Maybe.withDefault "not-done"

        attributes =
          [ onClick (TaskClicked task.descr)
          , class itemClass
          ]

        timeFormat t =
          Time.posixToMillis t
          |> Time.Format.format Time.utc "Weekday, Day Month Year at Hour:Minute:Second"

      in
        Grid.row
          [ Row.attrs [onClick (TaskClicked task.descr), class itemClass]]
          [ Grid.col [] [text task.descr]
          , Grid.col [] [text (timeFormat task.timeCreated)]
          , Grid.col [] [text (task.timeDone |> Maybe.map timeFormat |> Maybe.withDefault "in-progress")]
          ]

    header =
     [ Grid.row []
        [ Grid.col [] [text "Description"]
        , Grid.col [] [text "Created"]
        , Grid.col [] [text "Done"]
        ]
      ]

    taskList : List Task -> Html Msg
    taskList tasks = Grid.container [] (header ++ (List.map taskListItem tasks))

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
            , taskList model.tasks
            , newTaskArea
            , errorInfo
            ]
          ]
        ]
      ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    toggleDone : String -> List Task -> List Task
    toggleDone desc tasks =
      tasks
      |> List.map ( \t ->
          if t.descr == desc
            then
              { t
              | timeDone =  case t.timeDone of
                Just _ -> Nothing
                Nothing -> Just model.currentTime
              }
            else t
        )

    addNewTask () =
      ( { model
        | newTaskInput = ""
        , tasks =
            if String.isEmpty (String.trim model.newTaskInput)
              then model.tasks
              else model.tasks ++ [{descr = model.newTaskInput, timeCreated = model.currentTime, timeDone = Nothing} ]
        , error = Nothing
        }
      , Cmd.none
      )
  in
    case msg of
      TaskClicked desc ->
        ( { model | tasks = toggleDone desc model.tasks, error = Nothing}
        , Cmd.none
        )

      NewTaskInputChanged chg ->
        ( { model | newTaskInput = chg, error = Nothing}
        , Cmd.none
        )
      KeyUp key ->
        let
          isEnter = Keyboard.anyKeyUpper key
            |> Maybe.map (\k -> k == Keyboard.Enter)
            |> Maybe.withDefault False
          isNewTask = model.tasks |> List.filter (\t -> t.descr == model.newTaskInput) |> List.isEmpty
        in
          if isEnter && isNewTask
            then addNewTask ()
            else (model, Cmd.none)

      AddNewTask -> addNewTask ()

      PopulateTasks tasks ->
        ( { model | tasks = tasks, error = Nothing }
        , Cmd.none
        )

      ErrorMessage errMsg ->
        ({model | error = Just ("Error: " ++ errMsg)}
        , Cmd.none)

      Tick time ->
        ( { model | currentTime = time}
        , Cmd.none)

isTaskAlreadyAdded : List Task -> String -> Bool
isTaskAlreadyAdded tasks description =
  tasks
  |> List.filter (\t -> t.descr == description)
  |> List.isEmpty
  |> not


fetchTasks : Cmd Msg
fetchTasks =
  let
    backendUrl = "http://localhost:9000/tasks"

    decodeTime : Decode.Decoder Time.Posix
    decodeTime = Decode.int |> Decode.map Time.millisToPosix

    taskDecoder : Decode.Decoder Task
    taskDecoder =
      Decode.map3
        Task
        ( Decode.field "description" Decode.string)
        ( Decode.field "timeCreated" decodeTime)
        ( Decode.field "timeDone" (Decode.maybe decodeTime))

    handleDecoding : Result Error (List Task) -> Msg
    handleDecoding result = case result of
      Ok tasks -> PopulateTasks tasks
      Err _ -> ErrorMessage "Failed to get tasks"
  in
    Http.get
        { url = backendUrl
        , expect = expectJson handleDecoding (Decode.list taskDecoder)
        }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 Tick
    , Keyboard.ups KeyUp
    ]

main : Program () Model Msg
main = element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
