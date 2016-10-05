module Main exposing(..)
import Types exposing (..)
import Html exposing (table, button, text, tr, td, div)
import Html.Attributes exposing(disabled, height, width, attribute)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Array exposing (Array, fromList, get, indexedMap)
import Board exposing (makeMove)
import UI exposing (getGameHTML)
import Game exposing (getNewGameState, getMarker, gameOver, getCurrentPlayerType, getUpdatedGame)
import Computer

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
        Computer ->
          (update (MakeMove (Computer.getMove model)) model)
        Human ->
          model

    MakeMove spotIndex ->
      let updatedModel =
        {model | board = (makeMove model spotIndex (getMarker model)), isP1Turn = (not model.isP1Turn)}
      in
        if gameOver model then
          model
        else
          update (TakeTurn (getCurrentPlayerType updatedModel)) updatedModel

    StartGame gameType ->
      case gameType of
        HvH ->
          {model | status = InProgress, player1Type = Human, player2Type = Human}
        HvC ->
          {model | status = InProgress, player1Type = Human, player2Type = Computer}
        CvH ->
            (update (TakeTurn Computer) {model | status = InProgress, player1Type = Computer, player2Type = Human})
