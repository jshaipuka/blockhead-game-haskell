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

## Building And Running With Docker

```shell
docker build -t blockhead-game .
docker run -p 8080:8080 -d blockhead-game
```

## API

### Create New Field

#### Request

```shell
curl -v http://localhost:8080/api/field/5
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
curl -v -d '{ "field": [ ".....", ".....", "БАЛДА", ".....", "....." ], "usedWords": [ "БАЛДА" ],"difficulty":"Medium" }' http://localhost:8080/api/move-requests
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
  "success": true,
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

For DFS testing. It is possible to find word "ПЛАКАТИСТ" which consists of 9 letters if `difficulty` is set to `Hard`.

```shell
curl -v -d '{"field":[".....","АЛП..","КИСТЬ",".ТКАЖ","....."],"usedWords":["КИСТЬ","","КИСТА","","КИСКА","","ТАКТИК","","ТАКТИКА","","КАЛИТКА","","ЛИСТАЖ","","ПЛИТКА",""],"difficulty":"Hard"}' http://localhost:8080/api/move-requests
```

### Request

There are empty cells but computer cannot find any word.

```shell
curl -v -d '{"field":["ЬТЗСР","ЭСАПО","ГОГОТ","ТФАРТ",".ЬРТЫ"],"usedWords":["ГОГОТ","ТОГА","ФАГОТ","АГОГЭ","СОФА","САГА","САПОГ","ТОПАЗ","ЗАПОР","ЗАГАР","СПОРА","СПОРТ","ПОРТЫ","РОПОТ","РОПОТ","ТРОПА","ГАРЬ","ГОСТ","ПАСТЬ","РАФТ"],"difficulty":"Medium"}' http://localhost:8080/api/move-requests 
```

### Request

There are empty cells but computer cannot find any word, another example.

```shell
curl -v -d '{"field":["ТЬ.ЬН","ОНВДА","ЗАВОЗ","АГАЙЮ","ВАНСО"],"usedWords":["ЗАВОЗ","НАВОЗ","ЗАВОД","ЗАВОДЬ","ВДОВА","ДОЗА","НАВОЙ","НАДОЙ","ЗОНА","АЗОТ","ГАЗОН","ВАЗА","НАВАГА","ВАЗОН","ГАВАНЬ","НАГАН","АГАВА","АВАНС","ЮЗ","СОЮЗ"],"difficulty":"Medium"}' http://localhost:8080/api/move-requests
```

### Request

There are no empty cells.

```shell
curl -v -d '{ "field": [ "ТОНЯЯ", "ЕГТЫН", "БАЛДА", "ЕФЛАХ", "ДОПТА" ], "usedWords": [ "БАЛДА", "ФАЛДА", "БАЛЛ", "БАЛТ", "ГАЛЛ", "БЕГА", "БАЛЛАДА", "БАГЕТ", "АЛЛАХ", "НАХАЛ", "АЛТЫН", "ФАГОТ", "БЕТОН", "ПЛАХА", "ПТАХА", "ПЛАТА", "ОПЛАТА", "ДОПЛАТА", "ДЕБЕТ", "ТОНЯ", "ДЫНЯ" ],"difficulty":"Medium" }' http://localhost:8080/api/move-requests
```
