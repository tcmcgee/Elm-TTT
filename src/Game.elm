module Game exposing (..)
import Board exposing (getRows, getCols, getDiags)
import Array exposing(fromList, map, foldl, filter, length)
import List exposing (append, map, foldl)
import Types exposing(..)

getNewGameState =
  {board = fromList ["","","","","","","","",""], status = Menu, player1Type = Human, player1Marker = "X", player2Type = Human, player2Marker = "O", isXTurn = True}

getUpdatedGame model =
  if checkMarkerForWin model.board model.player1Marker then
    {model| status = (PlayerWins model.player1Marker)}
  else if checkMarkerForWin model.board model.player2Marker then
    {model| status = (PlayerWins model.player2Marker)}
  else if isTie model then
    {model| status = Tie}
  else if model.status == Menu then
    {model| status = Menu}
  else
    {model| status = InProgress}

getPossibleWins board =
  (append (append (getRows board) (getCols board)) (getDiags board))

checkMarkerForWin board marker =
  let possibleWins = getPossibleWins board
  in
    List.foldl (||) False (List.map (\x -> Array.foldl (&&) True x) (List.map (Array.map (\x -> x == marker)) possibleWins))

hasWinner {board, player1Marker, player2Marker} =
  (checkMarkerForWin board player1Marker) || (checkMarkerForWin board player2Marker)

isTie model =
  if hasWinner model then
    False
  else
    ((length (Array.filter (\x -> x == "") model.board)) == 0)

gameOver model =
  (||) (hasWinner model) (isTie model)

getCurrentPlayerType {isXTurn, player1Type, player2Type} =
  if isXTurn then
    player1Type
  else
    player2Type
