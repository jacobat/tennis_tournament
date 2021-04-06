port module Main exposing (..)

import Array
import Browser
import Game exposing (..)
import Html exposing (Html, button, div, input, table, td, text, tr)
import Html.Attributes exposing (class, name, style, value)
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
        [ td [ class "p-2" ]
            [ text ("Spiller " ++ (playerIdToInt player.playerId |> String.fromInt)) ]
        , td [ class "p-2" ]
            [ input [ class "filter drop-shadow-lg border border-gray-400 text-center rounded w-48 p-1", name "playerName", onInput (UpdatePlayerName (playerIdToInt player.playerId)), value player.name ] []
            ]
        , td [ class "p-2" ] [ text ("point: " ++ points) ]
        ]


playerListView : Model -> Html Msg
playerListView model =
    table [ class "mx-auto" ] (List.map (playerView model.games) model.players)


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
        [ td [ class "p-2" ] [ text (showPair players game.double1) ]
        , td [ class "p-2" ] [ input [ class "filter drop-shadow-lg border border-gray-400 text-center rounded w-12 p-1", onInput (UpdateScore1 game.gameId), value game.score1 ] [] ]
        , td [ class "p-2" ] [ text (showPair players game.double2) ]
        , td [] [ input [ class "filter drop-shadow-lg border border-gray-400 rounded text-center w-12 p-1", onInput (UpdateScore2 game.gameId), value game.score2 ] [] ]
        ]


gamesView : Model -> Html Msg
gamesView model =
    table [ class "mx-auto" ] (List.map (editGameView model.players) model.games)


view : Model -> Html Msg
view model =
    div [ class "container mx-auto max-w-screen-xl" ]
        [ div [ class "flex bg-blue-100 flex-col p-8" ]
            [ div [ class "flex-grow flex bg-blue-100" ]
                [ div [ class "flex-auto flex-grow" ]
                    [ playerListView model
                    ]
                , div [ class "flex-auto flex-grow" ]
                    [ gamesView model
                    ]
                ]
            , div [ class "mx-auto mt-8" ]
                [ button [ class "filter drop-shadow-lg border hover:bg-gray-300 bg-gray-100 py-2 px-4 rounded border border-black font-bold", onClick Reset ] [ text "Nulstil" ]
                ]
            ]
        ]
