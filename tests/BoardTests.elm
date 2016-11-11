module BoardTests exposing (..)

import Test exposing (..)
import Board exposing(..)
import Expect
import Array exposing(fromList, set)
import Types exposing (..)

gameState =
  {board = fromList [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty], status = Menu, player1Type = Human, player1Marker = X, player2Type = Human, player2Marker = O, isP1Turn = True}
emptyBoard = fromList [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]
fullBoard = fromList [X,O,X,O,X,X,O,O,X]

all : Test
all =
    describe "Board"
      [
      test "can get all the empty spots on a board" <|
         \() ->
           Expect.equal (getEmptySpots gameState.board) [0,1,2,3,4,5,6,7,8]

      , test "can get all the empty spots on a board that isn't empty" <|
         \() ->
           Expect.equal (getEmptySpots (fromList [X,O,X,X,X,X,Empty,Empty,Empty])) [6,7,8]

      , test "Can set the first board spot" <|
            \() ->
              Expect.equal (makeMove gameState 0 X) (fromList [X,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty])

        , test "can set a different board spot" <|
           \() ->
             Expect.equal (makeMove gameState 5 X) (fromList [Empty,Empty,Empty,Empty,Empty,X,Empty,Empty,Empty])

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
             Expect.equal (countOccupiedSpaces (fromList [X,O,X,Empty,Empty,Empty,Empty,Empty,Empty])) 3

        , test "gets the marker at the given index of the board" <|
           \() ->
             Expect.equal (getMarkerAt 0 (fromList [X,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty])) X

        , test "gets the marker at the given index of the empty board" <|
           \() ->
             Expect.equal (getMarkerAt 4 (fromList [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty])) Empty

        , test "gets the marker at the given index of the full board" <|
           \() ->
             Expect.equal (getMarkerAt 6 (fromList [X,O,X,O,X,X,O,X,X])) O

        , test "gets the rows of an empty board" <|
           \() ->
             Expect.equal (getRows gameState.board) [fromList [Empty,Empty,Empty],fromList [Empty,Empty,Empty],fromList [Empty,Empty,Empty]]

        , test "gets the rows of a non-empty board" <|
           \() ->
             Expect.equal (getRows (fromList [X,X,X,Empty,Empty,Empty,Empty,Empty,Empty] ))
                          [fromList [X,X,X],fromList [Empty,Empty,Empty],fromList [Empty,Empty,Empty]]

        , test "gets the rows in the correct order" <|
           \() ->
             Expect.equal (getRows (fromList [X,X,X,O,Empty,O,X,X,Empty] ))
                          [fromList [X,X,X],fromList [O,Empty,O],fromList [X,X,Empty]]

        , test "gets the cols of an empty board" <|
           \() ->
             Expect.equal (getCols gameState.board)
                          [fromList [Empty,Empty,Empty],fromList [Empty,Empty,Empty],fromList [Empty,Empty,Empty]]

        , test "gets the cols of a non-empty board" <|
           \() ->
             Expect.equal (getCols (fromList [X,Empty,Empty,X,Empty,Empty,X,Empty,Empty] ))
                          [fromList [X,X,X],fromList [Empty,Empty,Empty],fromList [Empty,Empty,Empty]]
                          
        , test "gets the cols in the correct order" <|
           \() ->
             Expect.equal (getCols (fromList [X,X,X,O,Empty,O,X,X,Empty] ))
                          [fromList [X,O,X],fromList [X,Empty,X],fromList [X,O,Empty]]

        , test "gets the diagonals of an empty board" <|
           \() ->
             Expect.equal (getDiags gameState.board)
                          [fromList [Empty,Empty,Empty],fromList [Empty,Empty,Empty]]

        , test "gets the diagonals of a non-empty board" <|
           \() ->
             Expect.equal (getDiags (fromList [X,Empty,Empty,X,Empty,Empty,X,Empty,Empty] ))
                          [fromList [X,Empty,Empty],fromList [X,Empty,Empty]]

        , test "gets the diagonals in the correct order" <|
           \() ->
             Expect.equal (getDiags (fromList [X,X,X,O,O,O,X,X,Empty] ))
                          [fromList [X,O,Empty],fromList [X,O,X]]
      ]
