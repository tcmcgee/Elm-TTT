module GameTests exposing (..)

import Test exposing (..)
import Game exposing (..)
import Expect
import Array exposing (fromList, set)
import Types exposing (..)


gameState =
    { board = fromList [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ], status = Menu, player1Type = Human, player1Marker = X, player2Type = Human, player2Marker = O, isP1Turn = True }


all : Test
all =
    describe "Game"
        [ test "Gets a new base game state" <|
            \() ->
                Expect.equal getNewGameState gameState
        , test "updating the game at the menu returns an unaltered gameState" <|
            \() ->
                Expect.equal (getUpdatedGame gameState) gameState
        , test "Gets the marker based on the current number of occupied board spaces for an empty board" <|
            \() ->
                Expect.equal (getMarker gameState) X
        , test "Gets the marker for a board when it's O's Turn " <|
            \() ->
                Expect.equal (getMarker { gameState | isP1Turn = False, board = (fromList [ X, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]) }) O
        , test "updating the game with a board with a player1 win sets the status to PlayerWins with the marker" <|
            \() ->
                Expect.equal (getUpdatedGame { gameState | board = (fromList [ X, X, X, Empty, Empty, Empty, Empty, Empty, Empty ]) }).status (PlayerWins gameState.player1Marker)
        , test "updating the game with a board with a player2 win sets the status to PlayerWins with the marker" <|
            \() ->
                Expect.equal (getUpdatedGame { gameState | board = (fromList [ O, O, O, Empty, Empty, Empty, Empty, Empty, Empty ]) }).status (PlayerWins gameState.player2Marker)
        , test "updating the game with a board with a tie sets the status to tie" <|
            \() ->
                Expect.equal (getUpdatedGame { gameState | board = (fromList [ O, X, O, O, X, O, X, O, X ]) }).status Tie
        , test "updating the game with a board that is in progress returns a board that is in progress" <|
            \() ->
                Expect.equal (getUpdatedGame { gameState | status = InProgress, board = (fromList [ O, Empty, Empty, Empty, Empty, Empty, X, Empty, Empty ]) }).status InProgress
        , test "gets the possible winning combinations of an empty board" <|
            \() ->
                Expect.equal (getPossibleWins gameState.board)
                    [ fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ] ]
        , test "gets the possible winning combinations of a non-empty board" <|
            \() ->
                Expect.equal (getPossibleWins (fromList [ X, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]))
                    [ fromList [ X, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ X, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ Empty, Empty, Empty ], fromList [ X, Empty, Empty ], fromList [ Empty, Empty, Empty ] ]
        , test "gets the possible winning combinations of a non-empty board in the correct order" <|
            \() ->
                Expect.equal (getPossibleWins (fromList [ X, X, O, O, X, X, O, X, O ]))
                    [ fromList [ X, X, O ], fromList [ O, X, X ], fromList [ O, X, O ], fromList [ X, O, O ], fromList [ X, X, X ], fromList [ O, X, O ], fromList [ X, X, O ], fromList [ O, X, O ] ]
        , test "can tell you if the game does not have a winner for a certain marker" <|
            \() ->
                Expect.equal (checkMarkerForWin gameState.board X) False
        , test "can tell you if the game has a winner for a certain marker if X wins" <|
            \() ->
                Expect.equal (checkMarkerForWin (fromList [ X, X, X, Empty, Empty, Empty, Empty, Empty, Empty ]) X) True
        , test "can tell you if the game is not over" <|
            \() ->
                Expect.equal (gameOver gameState) False
        , test "can tell you if the game is over" <|
            \() ->
                Expect.equal (gameOver { gameState | board = (fromList [ X, X, X, Empty, Empty, Empty, Empty, Empty, Empty ]) }) True
        , test "does not tell you that the game has a winner for a certain marker if the other marker wins" <|
            \() ->
                Expect.equal (checkMarkerForWin (fromList [ X, X, X, Empty, Empty, Empty, Empty, Empty, Empty ]) O) False
        , test "can tell you if the game has a winner for a certain marker if O wins" <|
            \() ->
                Expect.equal (checkMarkerForWin (fromList [ O, O, O, Empty, Empty, Empty, Empty, Empty, Empty ]) O) True
        , test "can tell you if the game does not have a winner" <|
            \() ->
                Expect.equal (hasWinner gameState) False
        , test "can tell you if the game does not have a winner with a full board" <|
            \() ->
                Expect.equal
                    (checkMarkerForWin
                        (fromList
                            [ O
                            , X
                            , O
                            , O
                            , X
                            , X
                            , X
                            , O
                            , O
                            ]
                        )
                        X
                    )
                    False
        , test "can tell you if the game has a winner if X wins" <|
            \() ->
                Expect.equal (hasWinner { gameState | board = (fromList [ X, X, X, Empty, Empty, Empty, Empty, Empty, Empty ]) }) True
        , test "can tell you if the game has a winner if O wins" <|
            \() ->
                Expect.equal (hasWinner { gameState | board = (fromList [ O, O, O, Empty, Empty, Empty, Empty, Empty, Empty ]) }) True
        , test "can tell you if the game has a winner if O wins diagnal" <|
            \() ->
                Expect.equal (hasWinner { gameState | board = (fromList [ O, Empty, Empty, Empty, O, Empty, Empty, Empty, O ]) }) True
        , test "can tell you if the game has a winner if O wins vertically" <|
            \() ->
                Expect.equal (hasWinner { gameState | board = (fromList [ O, Empty, Empty, O, Empty, Empty, O, Empty, Empty ]) }) True
        , test "can tell you if the game is not a tie if it is not" <|
            \() ->
                Expect.equal (isTie { gameState | board = (fromList [ O, Empty, Empty, O, Empty, Empty, O, Empty, Empty ]) }) False
        , test "can tell you if the game is a tie if it is" <|
            \() ->
                Expect.equal
                    (isTie
                        { gameState
                            | board =
                                (fromList
                                    [ O
                                    , X
                                    , O
                                    , O
                                    , X
                                    , X
                                    , X
                                    , O
                                    , O
                                    ]
                                )
                        }
                    )
                    True
        , test "can tell you if the game is not a tie if it isn't and the board is full" <|
            \() ->
                Expect.equal
                    (isTie
                        { gameState
                            | board =
                                (fromList
                                    [ O
                                    , X
                                    , X
                                    , O
                                    , X
                                    , X
                                    , X
                                    , O
                                    , O
                                    ]
                                )
                        }
                    )
                    False
        ]
