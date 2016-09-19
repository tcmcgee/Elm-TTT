module UI exposing (..)
import Html exposing (table, button, text, tr, td, div)
import Html.Attributes exposing(disabled, height, width, attribute)
import Html.Events exposing (onClick)
import Array exposing (fromList, get, indexedMap, map)

import Board exposing (..)
import Game exposing (..)

getButtonForIndex index board =
  button [onClick index, disabled (getMarkerAt index board /= "")] [text (getMarkerAt index board)]

getDisplayBoard board status =
  if status /= "in progress" then
    map (\x ->
      if x == "" then
        " "
      else
        x) board
  else
    board

getTurnText board status =
  if (status == "in progress") then
    (getMarker ({getGameState | board = board})) ++ "'s Turn!"
  else if status == "player1Wins" then
    "X Wins!!"
  else if status == "player2Wins" then
    "O Wins!!"
  else
    "Game Over, It's a Tie!"

getGame {board} status =
  let displayBoard = getDisplayBoard board status
  in
    div [attribute "margin-left" "auto", attribute "margin-right" "auto", attribute "align" "center"]
    [
      text (getTurnText board status),
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
          button [onClick -1] [text "Play Again"]
        ]
      ]
