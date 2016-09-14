module Tests exposing (..)

import Test exposing (..)
import Board exposing(..)
import UI exposing(..)
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
             Expect.equal (getTurnText emptyBoard) "X's Turn!"

        , test "gets the turns text for an empty board" <|
           \() ->
             Expect.equal (getTurnText (set 0 "X" emptyBoard)) "O's Turn!"
        -- , test "get game returns an html representation of the game" <|
        --    \() ->
        --     Expect.equal (getGame gameState)
      ]
