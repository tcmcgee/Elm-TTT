# Elm Tic Tac Toe [![Build Status](https://travis-ci.org/tcmcgee/Elm-TTT.svg?branch=master)](https://travis-ci.org/tcmcgee/Elm-TTT)

## To Play

### Requirements
* A Browser

### Instructions

* Navigate to [this website](http://www.tomcmcgee.me/Elm-TTT/) and play! (The deployed version has the first moves hard coded to improve play. Check out the branch `predetermined-first-moves` if you're interested in seeing that!)

OR

* Clone this repository and open `index.html` (in your browser of choice) and play!

**Note** The computer likes to take his time.

## To Develop

### Requirements

* Node
* Elm `npm install elm`
* Elm-test `npm install elm-test`

### Instructions

#### To Build
* `elm-make ./src/Main.elm --output=./dist/main.js`
* Open index.html

OR

* `elm-reactor` then navigate to http://localhost:8000/index.html

#### To Run Tests

* Run `npm install -g elm` (if you don't have elm installed)
* `npm install -g elm-test`
* Clone this repository
* `cd ./tests/`
* Run `elm package install`
* `cd ..`
* run `elm-test` to run the fast tests
* run `elm-test ./tests/LongMain.elm` to run every test (including the ones that call minimax)
