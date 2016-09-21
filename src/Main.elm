module Main exposing(..)
import Html exposing (table, button, text, tr, td, div)
import Html.Attributes exposing(disabled, height, width, attribute)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Array exposing (fromList, get, indexedMap)
import Computer
import Board exposing (..)
import UI exposing (..)
import Game exposing (..)
import Types exposing (..)

main =
  beginnerProgram { model = getNewGameState, view = view, update = update }

view model =
  getGameHTML (getUpdatedGame model)

update msg model =
  case msg of
    PlayAgain ->
      getNewGameState

    TakeTurn playerType ->
      case playerType of
        "Computer" ->
          (update (MakeMove (Computer.getMove model.board)) model)
        _ ->
          model

    MakeMove spotIndex ->
      let updatedModel =
        {model | board = (makeMove model spotIndex (getMarker model)), isXTurn = (not model.isXTurn)}
      in
        update (TakeTurn (getCurrentPlayerType updatedModel)) updatedModel

    StartGame gameType ->
      case gameType of
        HvH ->
          {model | status = InProgress, player1Type = "Human", player2Type = "Human"}
        HvC ->
          {model | status = InProgress, player1Type = "Human", player2Type = "Computer"}
        CvH ->
            (update (TakeTurn "Computer") {model | status = InProgress, player1Type = "Computer", player2Type = "Human"})
