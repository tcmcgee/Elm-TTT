module Computer exposing (..)
import Types exposing (..)
import Array exposing (toList)
import Board exposing (getNth, makeMove, getEmptySpots)
import Game exposing (hasWinner, isTie, getMarker)
import List exposing (reverse,sort, map, length)

getMove: GameState -> Int
getMove model =
  getBestMove model 0

getBestMove: GameState -> Int -> Int
getBestMove model depth =
  if hasWinner model then
    -10 + depth
  else if isTie model || depth > 5 then
    0
  else
    let openSpots = getEmptySpots model.board
        scoredSpots = (map (\openSpotIndex -> (-1 * getBestMove (updateGameState model openSpotIndex) (depth + 1))) openSpots)
    in
      if depth == 0 then
        getIndexOfMaxOfList scoredSpots 0 -100 -100 |>
          (\x -> getValueAtIndex x openSpots)
      else
        getListMax scoredSpots

updateGameState: GameState -> Int -> GameState
updateGameState model moveIndex =
  {model | board = (makeMove model moveIndex (getMarker model)), isP1Turn = (not model.isP1Turn)}

getIndexOfMaxList: List Int -> Int -> Int
getIndexOfMaxList list index =
  getIndexOfMaxOfList list index -100 -100

getIndexOfMaxOfList: List Int -> Int -> Int -> Int -> Int
getIndexOfMaxOfList list index maxIndex maxVal=
  case list of
    [] ->
      maxIndex
    val::vals ->
      if val > maxVal then
        getIndexOfMaxOfList vals (index + 1) index val
      else
        getIndexOfMaxOfList vals (index + 1) maxIndex maxVal

getListMax: List Int -> Int
getListMax list =
  Maybe.withDefault -1 (List.maximum list)

getValueAtIndex:Int -> List Int -> Int
getValueAtIndex index list =
  Maybe.withDefault -1 (Array.get index (Array.fromList list))
