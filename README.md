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
curl -v http://localhost:8080/api/field
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
curl -v --data '{ "field": [ ".....", ".....", "БАЛДА", ".....", "....." ], "usedWords": [ "БАЛДА" ] }' http://localhost:8080/api/move-requests
```

#### Response

```json
{
  "cell": [
    3,
    1
  ],
  "letter": "Ф",
  "path": [
    [
      3,
      1
    ],
    [
      2,
      1
    ],
    [
      2,
      2
    ],
    [
      2,
      3
    ],
    [
      2,
      4
    ]
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
curl -v --data '{ "field": [ "Т....", "ЕГТЫН", "БАЛДА", ".ФЛАХ", "....." ], "usedWords": [ "БАЛДА", "ФАЛДА", "БАЛЛ", "БАЛТ", "ГАЛЛ", "БЕГА", "БАЛЛАДА", "БАГЕТ", "АЛЛАХ", "НАХАЛ", "АЛТЫН" ] }' http://localhost:8080/api/move-requests
```

### Request

There are empty cells but computer cannot find any word.

```shell
curl -v --data '{"field":["ЬТЗСР","ЭСАПО","ГОГОТ","ТФАРТ",".ЬРТЫ"],"usedWords":["ГОГОТ","ТОГА","ФАГОТ","АГОГЭ","СОФА","САГА","САПОГ","ТОПАЗ","ЗАПОР","ЗАГАР","СПОРА","СПОРТ","ПОРТЫ","РОПОТ","РОПОТ","ТРОПА","ГАРЬ","ГОСТ","ПАСТЬ","РАФТ"]}' http://localhost:8080/api/move-requests 
```

### Request

There are no empty cells.

```shell
curl -v --data '{ "field": [ "ТОНЯЯ", "ЕГТЫН", "БАЛДА", "ЕФЛАХ", "ДОПТА" ], "usedWords": [ "БАЛДА", "ФАЛДА", "БАЛЛ", "БАЛТ", "ГАЛЛ", "БЕГА", "БАЛЛАДА", "БАГЕТ", "АЛЛАХ", "НАХАЛ", "АЛТЫН", "ФАГОТ", "БЕТОН", "ПЛАХА", "ПТАХА", "ПЛАТА", "ОПЛАТА", "ДОПЛАТА", "ДЕБЕТ", "ТОНЯ", "ДЫНЯ" ] }' http://localhost:8080/api/move-requests
```
