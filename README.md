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
  "cell": [ 3, 1 ],
  "letter": "Ф",
  "path": [
    [ 3, 1 ],
    [ 2, 1 ],
    [ 2, 2 ],
    [ 2, 3 ],
    [ 2, 4 ]
  ],
  "updatedField": [
    ".....",
    ".....",
    "БАЛДА",
    ".Ф...",
    "....."
  ],
  "word": "ФАЛДА"
}
```

### Request

```shell
curl -v --data '{ "field": [ "Т....", "ЕГТЫН", "БАЛДА", ".ФЛАХ", "....." ], "usedWords": [ "БАЛДА", "ФАЛДА", "БАЛЛ", "БАЛТ", "ГАЛЛ", "БЕГА", "БАЛЛАДА", "БАГЕТ", "АЛЛАХ", "НАХАЛ", "АЛТЫН" ] }' http://localhost:3000/api/move-requests
```

### Request

```shell
curl -v --data '{ "field": [ "ТОНЯЯ", "ЕГТЫН", "БАЛДА", "ЕФЛАХ", "ДОПТА" ], "usedWords": [ "БАЛДА", "ФАЛДА", "БАЛЛ", "БАЛТ", "ГАЛЛ", "БЕГА", "БАЛЛАДА", "БАГЕТ", "АЛЛАХ", "НАХАЛ", "АЛТЫН", "ФАГОТ", "БЕТОН", "ПЛАХА", "ПТАХА", "ПЛАТА", "ОПЛАТА", "ДОПЛАТА", "ДЕБЕТ", "ТОНЯ", "ДЫНЯ" ] }' http://localhost:3000/api/move-requests
```
