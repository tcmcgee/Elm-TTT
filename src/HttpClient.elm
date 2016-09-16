module HttpClient exposing (..)

import Http exposing (..)
import Json.Encode exposing (..)
import Json.Decode as Json exposing (..)
import Debug exposing(log)
import Array exposing(..)
import String exposing(..)
import Char exposing(..)
import Task exposing(..)

getURLWithParams url board =
  url ++ "?board=" ++ (toString board)

sendGetRequest : String -> a -> Task Http.Error (List (String, String))
sendGetRequest url board =
   Http.get status (getURLWithParams url board)

update msg =
  

status : Json.Decoder (List (String, String))
status =
  keyValuePairs Json.string
