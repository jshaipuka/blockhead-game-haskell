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

#### Request

```shell
curl -v http://localhost:3000/api/field
```

#### Response

```json
[
  ".....",
  ".....",
  "БАЛДА",
  ".....",
  "....."
]
```

### Ask Computer To Make A Move

#### Request

```shell
curl -v --data '{ "field": [ ".....", ".....", "БАЛДА", ".....", "....." ], "usedWords": [ "БАЛДА" ] }' http://localhost:3000/api/move-requests
```

#### Response

```json
{
  "letter": "Ф",
  "updatedField": [
    ".....",
    ".Ф...",
    "БАЛДА",
    ".....",
    "....."
  ],
  "word": "ФАЛДА",
  "x": 1,
  "y": 1
}
```