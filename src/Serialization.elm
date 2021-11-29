module Serialization exposing (todoListDecoder)
import Json.Decode
import Json.Encode
import Time
import Model exposing (Todo, Id(..))

decodeTime : Json.Decode.Decoder Time.Posix
decodeTime = Json.Decode.int |> Json.Decode.map Time.millisToPosix

decodeId = Json.Decode.string |> Json.Decode.map Id

todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
  Json.Decode.map4
    Todo
    ( Json.Decode.field "id" decodeId)
    ( Json.Decode.field "descr" Json.Decode.string)
    ( Json.Decode.field "timeCreated" decodeTime)
    ( Json.Decode.field "timeDone" (Json.Decode.maybe decodeTime))

todoListDecoder : Json.Decode.Decoder (List Todo)
todoListDecoder= Json.Decode.list todoDecoder