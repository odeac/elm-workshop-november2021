module View exposing (view)

import Model exposing (Model, Msg(..), Todo)
import Html exposing(Html, text, div, span, ul, li)
import Html.Attributes exposing(style)
import Html.Events exposing (onClick)
import Time
import Time.Format
import Maybe.Extra as Maybe

timeZone = Time.customZone 120 []

formatTime : Time.Posix -> String
formatTime t =
  Time.posixToMillis t
  |> Time.Format.format timeZone "Day Month Year at Hour:Minute:Second"


formatTodo : Todo -> String
formatTodo todo =
  "Descr: " ++ todo.descr
  ++ " -- Created: " ++ formatTime todo.timeCreated
  ++ (
    todo.timeDone
    |> Maybe.map (\t -> " -- Done: " ++ formatTime t)
    |> Maybe.withDefault " -- In progress"
  )

mkTodo : Todo -> Html Msg
mkTodo todo =
  let
    itemAttrs =
      [ onClick (Clicked todo.id todo.descr)
      ]
  in
    li itemAttrs [text <| formatTodo todo]

todoList model =
  [ ul []
    ( model.todos
      |> List.map mkTodo
    )
  ]


errorElem model =
  model.error
  |> Maybe.map (\error -> span [style "color" "red"] [text error])
  |> Maybe.toList

view : Model -> Html Msg
view model = div [] (todoList model ++ errorElem model)