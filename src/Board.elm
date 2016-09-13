port module Board exposing (..)
import Html exposing (..)
import Array exposing(get,set,length,filter, fromList)

getGameState =
  {board = fromList ["","","","","","","","",""]}

makeMove {board} move marker =
  set move marker board

countOccupiedSpaces board =
  (length (filter (\x -> x /= "") board))

getMarkerAt index board =
  Maybe.withDefault "" (get index board)

getMarker {board} =
  if (countOccupiedSpaces board) % 2 == 0 then
    "X"
  else
    "O"
