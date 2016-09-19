module Tests exposing (..)

import Test exposing (..)
import Board exposing(..)
import UI exposing(..)
import Game exposing(..)
import Expect
import Array exposing(fromList, set)
import String
import Html exposing (button, text, div)
import Html.Events exposing (onClick)
gameState =
  { board = fromList ["","","","","","","","",""] }
emptyBoard = fromList ["","","","","","","","",""]
fullBoard = fromList ["X","O","X","O","X","X","O","O","X"]
all : Test
all =
    describe "TicTacToe"
      [
      --- Board
        test "Can set the first board spot" <|
            \() ->
              Expect.equal (makeMove gameState 0 "X") (fromList ["X","","","","","","","",""])

        , test "can set a different board spot" <|
           \() ->
             Expect.equal (makeMove gameState 5 "X") (fromList ["","","","","","X","","",""])

        , test "Has a board Array" <|
           \() ->
             Expect.equal gameState.board emptyBoard

        , test "Can get the number of occupied spaces with an empty board" <|
           \() ->
             Expect.equal (countOccupiedSpaces emptyBoard) 0

        , test "can count the occupied spaces of a full board" <|
           \() ->
             Expect.equal (countOccupiedSpaces fullBoard) 9

        , test "can count the occupied spaces of a half full board" <|
           \() ->
             Expect.equal (countOccupiedSpaces (fromList ["X","O","X","","","","","",""])) 3

        , test "Gets the marker based on the current number of occupied board spaces for an empty board" <|
           \() ->
             Expect.equal (getMarker gameState) "X"

        , test "Gets the marker for a board when it's O's Turn " <|
           \() ->
             Expect.equal (getMarker {gameState| board = (fromList ["X","","","","","","","",""])}) "O"

        , test "gets the marker at the given index of the board" <|
           \() ->
             Expect.equal (getMarkerAt 0 (fromList ["X","","","","","","","",""])) "X"

        , test "gets the marker at the given index of the empty board" <|
           \() ->
             Expect.equal (getMarkerAt 4 (fromList ["","","","","","","","",""])) ""

        , test "gets the marker at the given index of the full board" <|
           \() ->
             Expect.equal (getMarkerAt 6 (fromList ["X","O","X","O","X","X","O","X","X"])) "O"

        , test "gets the rows of an empty board" <|
           \() ->
             Expect.equal (getRows gameState.board) [fromList ["","",""],fromList ["","",""],fromList ["","",""]]

        , test "gets the rows of a non-empty board" <|
           \() ->
             Expect.equal (getRows (fromList ["X","X","X","","","","","",""] ))
                          [fromList ["X","X","X"],fromList ["","",""],fromList ["","",""]]

        , test "gets the rows in the correct order" <|
           \() ->
             Expect.equal (getRows (fromList ["X","X","X","O","","O","X","X",""] ))
                          [fromList ["X","X","X"],fromList ["O","","O"],fromList ["X","X",""]]

        , test "gets the cols of an empty board" <|
           \() ->
             Expect.equal (getCols gameState.board)
                          [fromList ["","",""],fromList ["","",""],fromList ["","",""]]

        , test "gets the cols of a non-empty board" <|
           \() ->
             Expect.equal (getCols (fromList ["X","","","X","","","X","",""] ))
                          [fromList ["X","X","X"],fromList ["","",""],fromList ["","",""]]
        , test "gets the cols in the correct order" <|
           \() ->
             Expect.equal (getCols (fromList ["X","X","X","O","","O","X","X",""] ))
                          [fromList ["X","O","X"],fromList ["X","","X"],fromList ["X","O",""]]

        , test "gets the diagonals of an empty board" <|
           \() ->
             Expect.equal (getDiags gameState.board)
                          [fromList ["","",""],fromList ["","",""]]

        , test "gets the diagonals of a non-empty board" <|
           \() ->
             Expect.equal (getDiags (fromList ["X","","","X","","","X","",""] ))
                          [fromList ["X","",""],fromList ["X","",""]]

        , test "gets the diagonals in the correct order" <|
           \() ->
             Expect.equal (getDiags (fromList ["X","X","X","O","O","O","X","X",""] ))
                          [fromList ["X","O",""],fromList ["X","O","X"]]
        --- End Board
        --- UI
        , test "returns a button based on the index of the board if given 0" <|
           \() ->
             Expect.equal (getButtonForIndex 0 (fromList ["","","","","","","","",""])) (button [onClick 0] [text ""])

        , test "returns a dynamic button based on the index of the board if given 1" <|
           \() ->
             Expect.equal (getButtonForIndex 1 (fromList ["","","","","","","","",""])) (button [onClick 1] [text ""])

        , test "gets the turns text for an empty board" <|
           \() ->
             Expect.equal (getTurnText emptyBoard "in progress") "X's Turn!"

        , test "gets the turns text for an empty board" <|
           \() ->
             Expect.equal (getTurnText (set 0 "X" emptyBoard) "in progress") "O's Turn!"
        -- , test "get game returns an html representation of the game" <|
        --    \() ->
        --     Expect.equal (getGame gameState)

        --- End UI
        --- Game
        , test "gets the possible winning combinations of an empty board" <|
           \() ->
             Expect.equal (getPossibleWins gameState.board)
                          [fromList ["","",""], fromList ["","",""], fromList ["","",""], fromList ["","",""], fromList ["","",""], fromList ["","",""], fromList ["","",""], fromList ["","",""]]
        , test "gets the possible winning combinations of a non-empty board" <|
           \() ->
             Expect.equal (getPossibleWins (fromList ["X","","","","","","","",""]))
                          [fromList ["X","",""], fromList ["","",""], fromList ["","",""], fromList ["X","",""], fromList ["","",""], fromList ["","",""], fromList ["X","",""], fromList ["","",""]]

        , test "gets the possible winning combinations of a non-empty board in the correct order" <|
           \() ->
             Expect.equal (getPossibleWins (fromList ["0","1","2","3","4","5","6","7","8"]))
                          [fromList ["0","1","2"], fromList ["3","4","5"], fromList ["6","7","8"], fromList ["0","3","6"], fromList ["1","4","7"], fromList ["2","5","8"], fromList ["0","4","8"], fromList ["6","4","2"]]

        , test "can tell you if the game has a winner for a certain marker" <|
           \() ->
             Expect.equal (checkMarkerForWin gameState.board "X") False

        , test "can tell you if the game has a winner for a certain marker if X wins" <|
           \() ->
             Expect.equal (checkMarkerForWin (fromList ["X","X","X","","","","","",""]) "X") True

        , test "does not tell you that the game has a winner for a certain marker if the other marker wins" <|
           \() ->
             Expect.equal (checkMarkerForWin (fromList ["X","X","X","","","","","",""]) "O") False

        , test "can tell you if the game has a winner for a certain marker if O wins" <|
           \() ->
             Expect.equal (checkMarkerForWin (fromList ["O","O","O","","","","","",""]) "O") True

        , test "can tell you if the game does not have a winner" <|
           \() ->
             Expect.equal (hasWinner gameState.board) False

        , test "can tell you if the game does not have a winner with a full board" <|
           \() ->
              Expect.equal (checkMarkerForWin (fromList    ["O","X","O",
                                                  "O","X","X",
                                                  "X","O","O"]) "X") False

        , test "can tell you if the game has a winner if X wins" <|
           \() ->
             Expect.equal (hasWinner (fromList ["X","X","X","","","","","",""])) True

        , test "can tell you if the game has a winner if O wins" <|
           \() ->
             Expect.equal (hasWinner (fromList ["O","O","O","","","","","",""])) True

        , test "can tell you if the game has a winner if O wins diagnal" <|
           \() ->
             Expect.equal (hasWinner (fromList ["O","","","","O","","","","O"])) True

        , test "can tell you if the game has a winner if O wins vertically" <|
           \() ->
             Expect.equal (hasWinner (fromList ["O","","","O","","","O","",""])) True

        , test "can tell you if the game is not a tie if it is not" <|
           \() ->
             Expect.equal (isTie (fromList ["O","","","O","","","O","",""])) False

        , test "can tell you if the game is a tie if it is" <|
           \() ->
             Expect.equal (isTie (fromList ["O","X","O",
                                            "O","X","X",
                                            "X","O","O"])) True

        , test "can tell you if the game is not a tie if it isn't and the board is full" <|
           \() ->
             Expect.equal (isTie (fromList ["O","X","X",
                                            "O","X","X",
                                            "X","O","O"])) False
      ]
