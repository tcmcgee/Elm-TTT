module UITests exposing (..)

import Test exposing (..)
import UI exposing(..)
import Expect
import Array exposing(fromList, set)
import String
import Types exposing (..)
import Html exposing (button, text, div)
import Html.Events exposing (onClick)
gameState =
  {board = fromList [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty], status = Menu, player1Type = Human, player1Marker = X, player2Type = Human, player2Marker = O, isP1Turn = True}

all : Test
all =
    describe "UI"
      [
        test "returns a button based on the index of the board if given 0" <|
           \() ->
             Expect.equal (getButtonForIndex 0 (fromList ["","","","","","","","",""])) (button [onClick (MakeMove 0)] [text ""])

        , test "returns a dynamic button based on the index of the board if given 1" <|
           \() ->
             Expect.equal (getButtonForIndex 1 (fromList ["","","","","","","","",""])) (button [onClick (MakeMove 1)] [text ""])

        , test "gets the turns text for an empty board" <|
           \() ->
             Expect.equal (getTurnText InProgress X O True) "X's Turn!"

        , test "gets the turns text for a non-empty board" <|
           \() ->
             Expect.equal (getTurnText InProgress X O False) "O's Turn!"

        , test "gets the turns text when X wins" <|
           \() ->
             Expect.equal (getTurnText (PlayerWins X) X O True) "X Wins!!"

        , test "gets the turns text when O wins" <|
           \() ->
             Expect.equal (getTurnText (PlayerWins O) X O False) "O Wins!!"

        , test "gets the turns text when there's a tie" <|
           \() ->
             Expect.equal (getTurnText Tie X O True) "Game Over, It's a Tie!"

        , test "Gets the display board of an empty board and an in progress game" <|
           \() ->
             Expect.equal (getDisplayBoard gameState.board InProgress) (fromList ["", "", "", "", "", "", "", "", ""])

        , test "Gets the display board of a game that has a winner" <|
           \() ->
             Expect.equal (getDisplayBoard gameState.board (PlayerWins X)) (fromList [" ", " ", " ", " ", " ", " ", " ", " ", " "])

        , test "Gets the display board of a game that has a winner" <|
           \() ->
             Expect.equal (getDisplayBoard gameState.board (PlayerWins O)) (fromList [" ", " ", " ", " ", " ", " ", " ", " ", " "])

        , test "Gets the display board of a game that's a tie" <|
           \() ->
             Expect.equal (getDisplayBoard (fromList
             [X, O, X,
             O, X, O,
              O, X, O]) Tie)
              (fromList
             ["X", "O", "X",
             "O", "X", "O",
              "O", "X", "O"])
        --- End UI
      ]
