port module Main exposing (..)

import Test exposing (..)
import BoardTests
import UITests
import GameTests
import ComputerTests
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)



main : Program Value
main =
  run emit (describe "All the tests" [BoardTests.all, UITests.all, GameTests.all, ComputerTests.long])


port emit : ( String, Value ) -> Cmd msg
