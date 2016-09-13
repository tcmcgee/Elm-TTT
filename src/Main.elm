module Main exposing(..)
import Html exposing (table, button, text, tr, td, div)
import Html.Attributes exposing(disabled, height, width, attribute)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Array exposing (fromList, get, indexedMap)

import Board exposing (..)
import UI exposing (..)

main =
  beginnerProgram { model = getGameState, view = view, update = update }


view model =
  (getGame model)

update msg model =
     if msg == -1  then
       {model | board = (fromList ["","","","","","","","",""])}
      else
        {model | board = (makeMove model msg (getMarker model))}
