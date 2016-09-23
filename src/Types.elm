module Types exposing(..)
import Array exposing (..)

type PlayerType = Computer | Human
type alias Marker = String
type alias GameState = {board : Array String, status : Status, player1Type : PlayerType, player1Marker : Marker, player2Type : PlayerType, player2Marker : Marker, isXTurn : Bool}
type Msg = MakeMove Int | PlayAgain | StartGame GameType | TakeTurn PlayerType
type GameType = HvH | HvC | CvH
type Status = Menu | InProgress | PlayerWins Marker | Tie
