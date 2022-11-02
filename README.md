# Blockhead Game

## Running The Game

```shell
stack build --exec blockhead-game-exe
```

## Running The Unit Tests

```shell
stack test
```

## Loading A Module

```shell
stack ghci --ghci-options src/Lib.hs
```

Type `:quit` to exit once done.

## API

```shell
curl -v --data '{ "field": [ ".....", ".....", ".....", ".....", "....." ], "move": "Test" }' http://localhost:3000/api/games
```
