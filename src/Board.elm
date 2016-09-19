port module Board exposing (..)
import Html exposing (..)
import Array exposing(initialize, get,set,length,filter, fromList, slice, toList)
import List exposing(indexedMap, map, reverse)

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

getRows board =
  let boardSize = (length board)
      rowSize = round (sqrt (toFloat (length board)))
      rows = []
  in
      (slice 0 rowSize board)::
      (slice rowSize (rowSize + rowSize) board)::
      (slice (rowSize + rowSize) boardSize board)::rows

getDiags board =
  let rows = getRows board
  in
    (fromList (getDiagFromRows rows))::(fromList (getDiagFromRows (reverse rows)))::[]
    
getCols board =
  let rows = getRows board
  in
    (fromList (getColFromRows 0 rows))::(fromList (getColFromRows 1 rows))::(fromList (getColFromRows 2 rows))::[]

getDiagFromRows rows =
  indexedMap getNth rows

getColFromRows index rows =
  map (getNth index) rows

getNth index array =
  Maybe.withDefault "" (get index array)
