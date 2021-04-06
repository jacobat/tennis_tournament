port module Main exposing (..)

import Array
import Browser
import Game exposing (..)
import Html exposing (Html, button, div, input, table, td, text, tr)
import Html.Attributes exposing (name, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Model exposing (..)
import Player exposing (..)


init : String -> ( Model, Cmd Msg )
init flags =
    ( Model.decode flags
    , Cmd.none
    )


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = UpdatePlayerName Int String
    | UpdateScore1 GameId String
    | UpdateScore2 GameId String
    | Reset


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        UpdatePlayerName playerId name ->
            { model | players = List.map (updatePlayer (Player (PlayerId playerId) name)) model.players }

        UpdateScore1 gameId score ->
            { model | games = List.map (updateScore1 gameId score) model.games }

        UpdateScore2 gameId score ->
            { model | games = List.map (updateScore2 gameId score) model.games }

        Reset ->
            Model.defaultModel


serializedUpdate : Model -> ( Model, Cmd Msg )
serializedUpdate model =
    let
        sendSerializedModel =
            sendMessage (Encode.encode 0 (Model.encode model))
    in
    ( model, sendSerializedModel )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateModel msg model |> serializedUpdate


playerView : List Game -> Player -> Html Msg
playerView games player =
    let
        points =
            List.map (scoresForPlayer player) games
                |> List.map scoreToPoints
                |> List.sum
                |> String.fromInt
    in
    tr []
        [ td []
            [ text ("Spiller " ++ (playerIdToInt player.playerId |> String.fromInt)) ]
        , td []
            [ input [ name "playerName", onInput (UpdatePlayerName (playerIdToInt player.playerId)), value player.name ] []
            ]
        , td [] [ text ("point: " ++ points) ]
        ]


playerListView : Model -> Html Msg
playerListView model =
    table [] (List.map (playerView model.games) model.players)


showPair players ( player1id, player2id ) =
    let
        getPlayer playerId =
            List.filter (\player -> player.playerId == playerId) players |> List.head
    in
    case ( getPlayer player1id, getPlayer player2id ) of
        ( Just player1, Just player2 ) ->
            player1.name ++ " og " ++ player2.name

        _ ->
            "Fejl, kunne ikke finde spillere"


editGameView : List Player -> Game -> Html Msg
editGameView players game =
    let
        getPlayer playerId =
            List.filter (\player -> player.playerId == playerId) players |> List.head
    in
    tr []
        [ td [] [ text (showPair players game.double1) ]
        , td [] [ input [ onInput (UpdateScore1 game.gameId), value game.score1 ] [] ]
        , td [] [ text (showPair players game.double2) ]
        , td [] [ input [ onInput (UpdateScore2 game.gameId), value game.score2 ] [] ]
        ]


gamesView : Model -> Html Msg
gamesView model =
    table [] (List.map (editGameView model.players) model.games)


view : Model -> Html Msg
view model =
    div []
        [ table []
            [ tr []
                [ td [ style "width" "20em" ]
                    [ playerListView model
                    ]
                , td []
                    [ gamesView model
                    ]
                ]
            , tr []
                [ td []
                    [ button [ onClick Reset ] [ text "Nulstil" ]
                    ]
                ]
            ]
        ]
