module BoardTests exposing (..)

import Test exposing (..)
import Board exposing(..)
import Expect
import Array exposing(fromList, set)
import Types exposing (..)

gameState =
  {board = fromList ["","","","","","","","",""], status = Menu, player1Type = Human, player1Marker = "X", player2Type = Human, player2Marker = "O", isP1Turn = True}
emptyBoard = fromList ["","","","","","","","",""]
fullBoard = fromList ["X","O","X","O","X","X","O","O","X"]

all : Test
all =
    describe "Board"
      [
      test "can get all the empty spots on a board" <|
         \() ->
           Expect.equal (getEmptySpots gameState.board) [0,1,2,3,4,5,6,7,8]

      , test "can get all the empty spots on a board that isn't empty" <|
         \() ->
           Expect.equal (getEmptySpots (fromList ["X","O","X","X","X","X","","",""])) [6,7,8]

      , test "Can set the first board spot" <|
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
      ]
