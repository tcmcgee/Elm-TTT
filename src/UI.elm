module UI exposing (..)

import Types exposing (..)
import Html exposing (table, button, text, tr, td, div, h1, h2)
import Html.Attributes exposing (disabled, height, width, attribute)
import Html.Events exposing (onClick)
import Array exposing (Array, fromList, get, indexedMap, map)
import Maybe exposing (withDefault)
import Game exposing (getMarker, getNewGameState)


getButtonForIndex : Int -> Array String -> Html.Html Msg
getButtonForIndex index board =
    button [ onClick (MakeMove index), disabled ((withDefault "" (get index board)) /= "") ] [ text (withDefault "" (get index board)) ]


getMarkerText : Marker -> String
getMarkerText marker =
    case marker of
        X ->
            "X"

        O ->
            "O"

        Empty ->
            ""


getDisplayBoard : Array Marker -> Status -> Array String
getDisplayBoard board status =
    case status of
        InProgress ->
            map getMarkerText board

        _ ->
            map
                (\x ->
                    let
                        marker =
                            getMarkerText x
                    in
                        if marker == "" then
                            " "
                        else
                            marker
                )
                board


getTurnText : Status -> Marker -> Marker -> Bool -> String
getTurnText status player1Marker player2Marker isP1Turn =
    case status of
        InProgress ->
            (getMarkerText (getMarker ({ getNewGameState | player1Marker = player1Marker, player2Marker = player2Marker, isP1Turn = isP1Turn }))) ++ "'s Turn!"

        PlayerWins marker ->
            (getMarkerText marker) ++ " Wins!!"

        Tie ->
            "Game Over, It's a Tie!"

        Menu ->
            ""


getGameHTML : GameState -> Html.Html Msg
getGameHTML model =
    case model.status of
        Menu ->
            getMenuHTML model

        _ ->
            getBoardHTML model


getMenuHTML : GameState -> Html.Html Msg
getMenuHTML { board, status } =
    div [ attribute "margin-left" "auto", attribute "margin-right" "auto", attribute "align" "center" ]
        [ h1 [] [ text "Welcome To TicTacToe!!" ]
        , h2 [] [ text "Please select a game type..." ]
        , button [ onClick (StartGame HvH) ] [ text "Human vs Human" ]
        , button [ onClick (StartGame HvC) ] [ text "Human vs Computer" ]
        , button [ onClick (StartGame CvH) ] [ text "Computer vs Human" ]
        ]


getBoardHTML : GameState -> Html.Html Msg
getBoardHTML { board, status, player1Marker, player2Marker, isP1Turn } =
    let
        displayBoard =
            getDisplayBoard board status
    in
        div [ attribute "margin-left" "auto", attribute "margin-right" "auto", attribute "align" "center" ]
            [ h1 [] [ text (getTurnText status player1Marker player2Marker isP1Turn) ]
            , table []
                [ tr []
                    [ td []
                        [ (getButtonForIndex 0 displayBoard)
                        ]
                    , td []
                        [ (getButtonForIndex 1 displayBoard)
                        ]
                    , td []
                        [ (getButtonForIndex 2 displayBoard)
                        ]
                    ]
                , tr []
                    [ td []
                        [ (getButtonForIndex 3 displayBoard)
                        ]
                    , td []
                        [ (getButtonForIndex 4 displayBoard)
                        ]
                    , td []
                        [ (getButtonForIndex 5 displayBoard)
                        ]
                    ]
                , tr []
                    [ td []
                        [ (getButtonForIndex 6 displayBoard)
                        ]
                    , td []
                        [ (getButtonForIndex 7 displayBoard)
                        ]
                    , td []
                        [ (getButtonForIndex 8 displayBoard)
                        ]
                    ]
                ]
            , div []
                [ button [ onClick PlayAgain ] [ text "Play Again" ]
                ]
            ]
