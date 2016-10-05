port module Board exposing (..)
import Html exposing (..)
import Types exposing (..)
import Array exposing(Array, initialize, get,set,length,filter, fromList, slice, toList, indexedMap)
import List exposing(indexedMap, map, reverse)

makeMove: GameState -> Int -> Marker -> Array Marker
makeMove {board} move marker =
  set move marker board

getEmptySpots: Array Marker -> List Int
getEmptySpots board =
  (Array.indexedMap (\x y ->
                      if (y == Empty) then
                        x
                      else
                        -1
                        ) board) |> filter (\x -> x /= -1) |> toList

countOccupiedSpaces: Array Marker -> Int
countOccupiedSpaces board =
  (length (filter (\x -> x /= Empty) board))

getMarkerAt: Int -> Array.Array Marker -> Marker
getMarkerAt index board =
  Maybe.withDefault Empty (get index board)

getRows: Array Marker -> List (Array Marker)
getRows board =
  let boardSize = (length board)
      rowSize = round (sqrt (toFloat (length board)))
      rows = []
  in
      (slice 0 rowSize board)::
      (slice rowSize (rowSize + rowSize) board)::
      (slice (rowSize + rowSize) boardSize board)::rows

getDiags: Array Marker -> List (Array Marker)
getDiags board =
  let rows = getRows board
  in
    (fromList (getDiagFromRows rows))::(fromList (getDiagFromRows (reverse rows)))::[]

getCols: Array Marker -> List (Array Marker)
getCols board =
  let rows = getRows board
  in
    (fromList (getColFromRows 0 rows))::(fromList (getColFromRows 1 rows))::(fromList (getColFromRows 2 rows))::[]

getDiagFromRows: List (Array Marker) -> List Marker
getDiagFromRows rows =
  List.indexedMap getNth rows

getColFromRows: Int -> List (Array Marker) -> List Marker
getColFromRows index rows =
  map (getNth index) rows

getNth: Int -> Array Marker -> Marker
getNth index array =
  Maybe.withDefault Empty (get index array)
