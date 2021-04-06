module Game exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Player exposing (..)


type GameId
    = GameId Int


type alias Game =
    { gameId : GameId
    , double1 : ( PlayerId, PlayerId )
    , double2 : ( PlayerId, PlayerId )
    , score1 : String
    , score2 : String
    }


initGame id player1 player2 player3 player4 =
    Game (GameId id) ( PlayerId player1, PlayerId player2 ) ( PlayerId player3, PlayerId player4 ) "" ""


initGames : List Player -> List Game
initGames players =
    -- runde 1
    [ initGame 1 1 2 3 4
    , initGame 2 5 6 7 8

    -- runde 2
    , initGame 3 1 3 2 4
    , initGame 4 5 7 6 8

    -- runde 3
    , initGame 5 1 4 2 3
    , initGame 6 5 8 6 7

    -- runde 4
    , initGame 7 1 5 2 6
    , initGame 8 3 7 4 8

    -- runde 5
    , initGame 9 1 6 2 5
    , initGame 10 3 8 4 7

    -- runde 6
    , initGame 11 1 7 2 8
    , initGame 12 3 5 4 6

    -- runde 7
    , initGame 13 1 8 4 5
    , initGame 14 2 7 3 6
    ]


updateScore1 : GameId -> String -> Game -> Game
updateScore1 gameId score game =
    if game.gameId == gameId then
        { game | score1 = score }

    else
        game


updateScore2 : GameId -> String -> Game -> Game
updateScore2 gameId score game =
    if game.gameId == gameId then
        { game | score2 = score }

    else
        game


scoresForPlayer : Player -> Game -> String
scoresForPlayer player game =
    let
        partOfDouble1 =
            tupleToList game.double1 |> List.member player.playerId

        partOfDouble2 =
            tupleToList game.double2 |> List.member player.playerId
    in
    if partOfDouble1 then
        game.score1

    else if partOfDouble2 then
        game.score2

    else
        ""


scoreToPoints scoreString =
    let
        score =
            String.toInt scoreString |> Maybe.withDefault 0
    in
    if score >= 10 then
        6

    else if score >= 8 then
        4

    else if score >= 6 then
        3

    else if score >= 4 then
        2

    else if score >= 1 then
        1

    else
        0


tupleToList : ( a, a ) -> List a
tupleToList tuple =
    [ Tuple.first tuple, Tuple.second tuple ]


encode : Game -> Encode.Value
encode game =
    let
        gId =
            case game.gameId of
                GameId id ->
                    id

        double1 =
            tupleToList game.double1 |> List.map playerIdToInt

        double2 =
            tupleToList game.double2 |> List.map playerIdToInt
    in
    Encode.object
        [ ( "gameId", Encode.int gId )
        , ( "double1", Encode.list Encode.int double1 )
        , ( "double2", Encode.list Encode.int double2 )
        , ( "score1", Encode.string game.score1 )
        , ( "score2", Encode.string game.score2 )
        ]


decodeGameId =
    Decode.map GameId Decode.int


decodeTuple =
    Decode.list Decode.int



-- arrayAsTuple2 : Decode a -> Decode b -> Decode ( a, b )


arrayAsTuple2 a b =
    Decode.index 0 a
        |> Decode.andThen
            (\aVal ->
                Decode.index 1 b
                    |> Decode.andThen (\bVal -> Decode.succeed ( PlayerId aVal, PlayerId bVal ))
            )


decode =
    Decode.map5 Game
        (Decode.field "gameId" decodeGameId)
        (Decode.field "double1" (arrayAsTuple2 Decode.int Decode.int))
        (Decode.field "double2" (arrayAsTuple2 Decode.int Decode.int))
        (Decode.field "score1" Decode.string)
        (Decode.field "score2" Decode.string)
