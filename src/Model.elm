module Model exposing (..)

import Time

type Id = Id String
type alias Todo =
  { id : Id
  , descr : String
  , timeCreated : Time.Posix
  , timeDone : Maybe Time.Posix
  }
type Model
  = Page1
    { todos : List Todo
    , error : Maybe String
    , now : Time.Posix
    }
  | Page2 {...}

type alias Flags = ()

type MsgPage1
  = Clicked Id String
  | DoneConfirmation Id Bool
  | PopulateTodos (List Todo)
  | ErrorMessage String
  | Tick Time.Posix

type MsgPage2 = ()

type Msg
  = MsgPage1 MsgPage1
  | MsgPage2 MsgPage2