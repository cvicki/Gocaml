# Go Games

Here we document what constitues a valid JSON Go game file.
Note, when adding new game files to this directory, please include an image of what the board should look like.
While the formatting for saved games is a little off, `Shift` + `Option` + `F` on Mac or `Shift` + `Alt` + `F` on Windows will autoformat the file.

## General Format

There are three required properties in the first level of the JSON file: `players`, `board`, and `config`:
```
{
  "players" : {},
  "board" : {},
  "config" : {}
}
```

## Players

`p#` is the player number. This is either `p1` for player 1 or `p2` for player 2. A valid game must include both `p1` and `p2` keys.
`byoyomi` is the number of byoyomi periods remaining for that player. Once all periods are used, the player loses on time.
`game_time` is the time remaining on that player's main time clock.
`id` is the name of the player.
`prisoners` is the list of all the stones captured. More details are below.
By default, player 1, or `p1`, plays with black stones and starts the game.

```
p# : {
  "byoyomi" : int,
  "game_time" : int,
  "id" : string,
  "prisoners" : prisoner list 
}
```

### Prisoner

A single prisoner is represented by three fields `col`, `row`, and `cur_stones`. The `col` and `row` fields denote the column and row of the stone that was captured. The field `cur_stones` represents the number of stones on the board after the capture is completed.
```
{
  "col" : int,
  "row" : int,
  "cur_stones" : int
}
```

## Board

`size` must be an odd integer on the interval `[3, 25)`. While most go boards are either 9x9, 13x13, or 19x19, we are allowing for more customization in this field. 
`white` and `black` are arays consisting of all the moves made by the two players. More details are below.
```
{
  "size" : int,
  "white" : move array,
  "black" : move array
}
```

### Move

A single move is represented by three fields, `col`, `row`, and `cur_stones`. The column and row are the column and row the stone was placed on. Columns and rows are index zero based with the origin at the top left corner. This value allows for an easier detection of ko violations later on in the game. The `cur_stones` represents the number of stones on the board after the move is completed.
```
{
  "col" : int,
  "row" : int,
  "cur_stones" : int
}
```

## Config

`byoyomi_period` is the number of seconds in a single byoyomi period.
`komi` is a positive real number and represents the compensation awarded to the player who went second in the game (customarily white). This is usually a non-integer value to avoid ties.
`turn` denotes the player whose turn it is. This must either be `"w"` or `"b"`, representing White's or Black's turn.
```
{
  "byoyomi_period" : int,
  "komi" : float,
  "turn" : string
}
```
