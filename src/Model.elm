module Model exposing (..)

import Game exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Player exposing (..)


type alias Model =
    { players : List Player
    , games : List Game
    }


defaultModel =
    Model Player.default (Game.initGames Player.default)


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "games", Encode.list Game.encode model.games )
        , ( "players", Encode.list Player.encode model.players )
        ]


decoder =
    Decode.map2 Model
        (Decode.field "players" (Decode.list Player.decode))
        (Decode.field "games" (Decode.list Game.decode))


decode : String -> Model
decode json =
    case
        Decode.decodeString decoder json
    of
        Ok model ->
            model

        Err e ->
            defaultModel
