module Computer exposing (..)
import Array exposing (toList)
import Board exposing (getNth)

getMove board =
  checkSpot board 0

checkSpot board index =
  case getNth index board of
    "" ->
      index
    _ ->
      checkSpot board (index + 1)
