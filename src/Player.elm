module Player exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type PlayerId
    = PlayerId Int


type alias Player =
    { playerId : PlayerId
    , name : String
    }


type alias NewPlayer =
    { playerId : Int
    , name : String
    }


initPlayers : List Player
initPlayers =
    List.repeat 8 "" |> List.indexedMap initPlayer


default =
    List.repeat 8 "" |> List.indexedMap initDefaultPlayer


initDefaultPlayer id name =
    Player (PlayerId (id + 1)) ""


initPlayer id name =
    Player (PlayerId (id + 1)) (getName id)


encode player =
    Encode.object
        [ ( "playerId", Encode.int (playerIdToInt player.playerId) )
        , ( "name", Encode.string player.name )
        ]


playerIdDecoder =
    Decode.map PlayerId Decode.int


decode =
    Decode.map2 Player
        (Decode.field "playerId" playerIdDecoder)
        (Decode.field "name" Decode.string)


updatePlayer : Player -> Player -> Player
updatePlayer currentPlayer player =
    if currentPlayer.playerId == player.playerId then
        { player | name = currentPlayer.name }

    else
        player


getName id =
    [ "Henriette"
    , "Christian"
    , "Jesper H."
    , "Simon"
    , "Line"
    , "Mikki"
    , "Emil"
    , "Teresa"
    ]
        |> List.drop id
        |> List.head
        |> Maybe.withDefault "Fejl"


playerIdToInt : PlayerId -> Int
playerIdToInt playerId =
    case playerId of
        PlayerId id ->
            id
