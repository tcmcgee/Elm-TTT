module Types exposing(..)
import Array exposing(Array)

type PlayerType = Computer | Human
type Marker = X | O | Empty
type alias GameState = {board : Array Marker, status : Status, player1Type : PlayerType, player1Marker : Marker, player2Type : PlayerType, player2Marker : Marker, isP1Turn : Bool}
type Msg = MakeMove Int | PlayAgain | StartGame GameType | TakeTurn PlayerType
type GameType = HvH | HvC | CvH
type Status = Menu | InProgress | PlayerWins Marker | Tie
