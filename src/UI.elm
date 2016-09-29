module UI exposing (..)
import Html exposing (table, button, text, tr, td, div, h1, h2)
import Html.Attributes exposing(disabled, height, width, attribute)
import Html.Events exposing (onClick)
import Array exposing (fromList, get, indexedMap, map)

import Board exposing (..)
import Game exposing (..)
import Types exposing (..)

getButtonForIndex index board =
  button [onClick (MakeMove index), disabled (getMarkerAt index board /= "")] [text (getMarkerAt index board)]

getDisplayBoard board status =
  case status of
    InProgress ->
      board
    _ ->
      map (\x ->
        if x == "" then
          " "
        else
          x) board

getTurnText status player1Marker player2Marker isP1Turn =
  case status of
    InProgress ->
      (getMarker ({getNewGameState | player1Marker = player1Marker, player2Marker = player2Marker, isP1Turn = isP1Turn})) ++ "'s Turn!"
    PlayerWins marker ->
      marker ++ " Wins!!"
    Tie ->
      "Game Over, It's a Tie!"
    Menu ->
      ""

getGameHTML model =
  case model.status of
    Menu ->
      getMenuHTML model
    _ ->
      getBoardHTML model

getMenuHTML {board,status} =
  div [attribute "margin-left" "auto", attribute "margin-right" "auto", attribute "align" "center"]
  [
    h1 [] [text "Welcome To TicTacToe!!"],
    h2 [] [text "Please select a game type..."],
    button [onClick (StartGame HvH) ] [text "Human vs Human"],
    button [onClick (StartGame HvC) ] [text "Human vs Computer"],
    button [onClick (StartGame CvH) ] [text "Computer vs Human"]
  ]

getBoardHTML {board,status,player1Marker,player2Marker,isP1Turn} =
  let displayBoard = getDisplayBoard board status
  in
    div [attribute "margin-left" "auto", attribute "margin-right" "auto", attribute "align" "center"]
    [
      h1[] [text (getTurnText status player1Marker player2Marker isP1Turn)],
      table []
        [
          tr[] [
            td[] [
            (getButtonForIndex 0 displayBoard)],
            td[] [
            (getButtonForIndex 1 displayBoard)],
            td[] [
            (getButtonForIndex 2 displayBoard)]
          ],
          tr[] [
            td[] [
            (getButtonForIndex 3 displayBoard)],
            td[] [
            (getButtonForIndex 4 displayBoard)],
            td[] [
            (getButtonForIndex 5 displayBoard)]
          ],
        tr[] [
          td[] [
          (getButtonForIndex 6 displayBoard)],
          td[] [
          (getButtonForIndex 7 displayBoard)],
          td[] [
          (getButtonForIndex 8 displayBoard)]
        ]
        ],
        div []
        [
          button [onClick PlayAgain] [text "Play Again"]
        ]
      ]
