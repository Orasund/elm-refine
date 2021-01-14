module Data.IncomingMsg exposing (IncomingMsg, fromTuple)

import Json.Decode as D exposing (Decoder)


type alias IncomingMsg =
    { kind : String
    , payload : String
    }


fromTuple : ( String, String ) -> IncomingMsg
fromTuple ( kind, payload ) =
    { kind = kind
    , payload = payload
    }
