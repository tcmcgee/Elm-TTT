module UI exposing (..)
import Html exposing (table, button, text, tr, td, div)
import Html.Attributes exposing(disabled, height, width, attribute)
import Html.Events exposing (onClick)
import Array exposing (fromList, get, indexedMap)

import Board exposing (..)
import Game exposing (..)

getButtonForIndex index board =
  button [onClick index, disabled (getMarkerAt index board /= "")] [text (getMarkerAt index board)]


getTurnText board status =
  if (status == "in progress") then  
    (getMarker ({getGameState | board = board})) ++ "'s Turn!"
  else
    "Winner!"

getGame {board, status} =
  div [attribute "margin-left" "auto", attribute "margin-right" "auto", attribute "align" "center"]
  [
    text (getTurnText board status),
    table []
      [
        tr[] [
          td[] [
          (getButtonForIndex 0 board)],
          td[] [
          (getButtonForIndex 1 board)],
          td[] [
          (getButtonForIndex 2 board)]
        ],
        tr[] [
          td[] [
          (getButtonForIndex 3 board)],
          td[] [
          (getButtonForIndex 4 board)],
          td[] [
          (getButtonForIndex 5 board)]
        ],
      tr[] [
        td[] [
        (getButtonForIndex 6 board)],
        td[] [
        (getButtonForIndex 7 board)],
        td[] [
        (getButtonForIndex 8 board)]
      ]
      ],
      div []
      [
        button [onClick -1] [text "Play Again"]
      ]
    ]
