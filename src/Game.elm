module Game exposing (..)
import Types exposing(..)
import Board exposing (getRows, getCols, getDiags)
import Array exposing(Array, fromList, map, foldl, filter, length)
import List exposing (append, map, foldl)

getNewGameState: GameState
getNewGameState =
  {board = fromList [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty], status = Menu, player1Type = Human, player1Marker = X, player2Type = Human, player2Marker = O, isP1Turn = True}

getUpdatedGame: GameState -> GameState
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

getMarker: GameState -> Marker
getMarker {player1Marker, player2Marker, isP1Turn} =
  if isP1Turn then
    player1Marker
  else
    player2Marker

getPossibleWins: Array Marker -> List (Array Marker)
getPossibleWins board =
  (append (append (getRows board) (getCols board)) (getDiags board))

checkMarkerForWin: Array Marker -> Marker -> Bool
checkMarkerForWin board marker =
  let possibleWins = getPossibleWins board
  in
    List.foldl (||) False (List.map (\x -> Array.foldl (&&) True x) (List.map (Array.map (\x -> x == marker)) possibleWins))

hasWinner: GameState -> Bool
hasWinner {board, player1Marker, player2Marker} =
  (checkMarkerForWin board player1Marker) || (checkMarkerForWin board player2Marker)

isTie: GameState -> Bool
isTie model =
  if hasWinner model then
    False
  else
    ((length (Array.filter (\x -> x == Empty) model.board)) == 0)

gameOver: GameState -> Bool
gameOver model =
  (||) (hasWinner model) (isTie model)

getCurrentPlayerType: GameState  -> PlayerType
getCurrentPlayerType {isP1Turn, player1Type, player2Type} =
  if isP1Turn then
    player1Type
  else
    player2Type
