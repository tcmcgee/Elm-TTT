module Computer exposing (..)
import Array exposing (toList)
import Board exposing (getNth, makeMove, getEmptySpots)
import Game exposing (hasWinner, isTie, getMarker)
import List exposing (reverse,sort, map, length)


getMove model =
  playAllGames model 0

playAllGames model depth =
  if hasWinner model then
    -10 + depth
  else if isTie model then
    0
  else
    let openSpots = getEmptySpots model.board
        scoredSpots = (map (\x -> (-1 * playAllGames {model | board = (makeMove model x (getMarker model)), isP1Turn = (not model.isP1Turn)} (depth + 1))) openSpots)
    in
      if depth == 0 then
        getIndexOfMaxOfList scoredSpots 0 -100 -100 |>
          (\x -> getValueAtIndex x openSpots)
      else
        getListMax scoredSpots

getIndexOfMaxOfList list index maxIndex maxVal=
  case list of
    [] ->
      maxIndex
    val::vals ->
      if val > maxVal then
        getIndexOfMaxOfList vals (index + 1) index val
      else
        getIndexOfMaxOfList vals (index + 1) maxIndex maxVal

getListMax list =
  Maybe.withDefault -1 (List.maximum list)

getValueAtIndex index list =
  Maybe.withDefault -1 (Array.get index (Array.fromList list))
