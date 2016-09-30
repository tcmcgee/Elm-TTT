module ComputerTests exposing (..)

import Test exposing (..)
import Computer exposing(..)
import Expect
import Array exposing(fromList, set)
import Types exposing (..)
gameState =
  {board = fromList ["","","","","","","","",""], status = Menu, player1Type = Human, player1Marker = "X", player2Type = Human, player2Marker = "O", isP1Turn = True}

all : Test
all =
    describe "Computer"
      [
        test "can get the index of the max of a list" <|
           \() ->
             Expect.equal (getIndexOfMaxOfList [1,2,3,4] 0 -1 -1) 3

        , test "can get the index of the max of the 2nd element of a list" <|
           \() ->
             Expect.equal (getIndexOfMaxOfList [1,5,3,4] 0 -1 -1) 1

        , test "can get the max of a list" <|
           \() ->
             Expect.equal (getListMax [1,2,3,4,5,4,3,2,2]) 5

        , test "can get the value of a list of ints at a given index" <|
           \() ->
             Expect.equal (getValueAtIndex 2 [0,1,2,3,4,5]) 2

        , test "can get the value of a list of ints at a given index if it's the first element" <|
           \() ->
             Expect.equal (getValueAtIndex 0 [0,1,2,3,4,5]) 0

        , test "can get the value of a list of ints at a given index if it's the first element" <|
           \() ->
             Expect.equal (getValueAtIndex 4 [0,1,2,3,4,5]) 4

        , test "can change the turn on a board when it updates the game state" <|
           \() ->
             Expect.equal (updateGameState gameState 0).isP1Turn False

        , test "can change the turn on a board when it updates the game state" <|
           \() ->
             Expect.equal (updateGameState {gameState| isP1Turn = False} 0).isP1Turn True

        , test "can update the board when it updates the game state" <|
           \() ->
             Expect.equal (updateGameState gameState 0).board (fromList ["X","","","","","","","",""])

        , test "can update the board with player2Marker when it updates the game state" <|
           \() ->
             Expect.equal (updateGameState {gameState | isP1Turn = False} 0).board (fromList ["O","","","","","","","",""])
        , test "can return a tie score (0) if the depth is ever greater than 4" <|
           \() ->
             Expect.equal (playAllGames gameState 6) 0
      ]

long : Test
long =
    describe "TicTacToe"
      [
        all
        , test "It can return a corner when the board is empty" <|
           \() ->
             Expect.equal (getMove {gameState | board = (fromList ["","","","","","","","",""])}) 0

        , test "It can return the middle if player1 takes a corner" <|
           \() ->
             Expect.equal (getMove {gameState | board = (fromList ["X","","","","","","","",""]), isP1Turn = False}) 4

        , test "can play every possible game and win given the opportunity" <|
           \() ->
             Expect.equal (playAllGames {gameState | board = (fromList ["X","O","X",
                        "O","X","X",
                        "O","",""]), isP1Turn = True} 0 ) 8

        , test "can play every possible game and blocks a possible win" <|
           \() ->
             Expect.equal (getMove {gameState | board = (fromList ["X","X","O",
                        "O","X","O",
                        "","",""]), isP1Turn = False}) 8
      ]
