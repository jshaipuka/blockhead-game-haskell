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

### Create New Field

```shell
curl -v http://localhost:3000/api/field
```

### Ask Computer To Make A Move

```shell
curl -v --data '{ "field": [ ".....", ".....", "СКУНС", ".....", "....." ], "usedWords": [ "СКУНС" ] }' http://localhost:3000/api/move-requests
```
