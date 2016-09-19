module Game exposing (..)
import Board exposing (getRows, getCols, getDiags)
import Array exposing(fromList, map, foldl, filter, length)
import List exposing (append, map, foldl)

getGameState =
  {board = fromList ["","","","","","","","",""], status = "in progress"}

getGameStatus board =
  if checkMarkerForWin board "X" then
    "player1Wins"
  else if checkMarkerForWin board "O" then
    "player2Wins"
  else if isTie board then
    "tie"
  else
    "in progress"

getPossibleWins board =
  (append (append (getRows board) (getCols board)) (getDiags board))

checkMarkerForWin board marker =
  let possibleWins = getPossibleWins board
  in
    List.foldl (||) False (List.map (\x -> Array.foldl (&&) True x) (List.map (Array.map (\x -> x == marker)) possibleWins))

hasWinner board =
  (checkMarkerForWin board "X") || (checkMarkerForWin board "O")

isTie board =
  if hasWinner board then
    False
  else
    ((length (Array.filter (\x -> x == "") board)) == 0)
