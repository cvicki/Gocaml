# GOcaml
An OCaml implementation of the game Go

## How to Play

Many game rules adapted from [here](https://www.britgo.org/intro/intro2.html).


#### Setup
###### Board
Board size is square. Typical sizes are 19 x 19, 13 x 13, and 9 x 9. The board is empty at the start of the game. 
###### Stones
Each player gets an infinite number of stones to use. Stones are either Black or White. 


#### Game play
###### Who goes first?
The weaker player gets the black pieces and goes first. There is a handicap system in place. The difference in rank of the two players is found and black places that many stones on the larger points on the board as their first turn. 
###### Timing
The [Byo-yomi timing system](https://senseis.xmp.net/?ByoYomi) will be used. Each player is given a number of time periods (byoyomi), each of certain length (byoyomi_period). For each turn, the time a player takes is timed and then each full time period that is used is removed, otherwise the player keeps the period. When a player runs out of periods, it is also considered a loss. 

Example: 

5 byoyomi periods each of length 2 minutes. 

If a player takes less than 2 minutes, they keep all 5 periods. 

If they take 2.5 minutes, they lose a period and have 4 periods remaining. 

If they take 5.9 minutes, they have 3 periods remaining. 
###### Game play
Players take turns placing stones on empty points of the board. Stones can be place anywhere except for two cases, Ko (see below) and self-capture. 
Self-capture is defined as placing a stone on a point with no liberties (see below). 

However, it is allowed if by placing the stone, one of the surrounding stones is captured and the stone now has liberties.  
###### How the game ends? 
The game ends after two consecutive passes. To pass, a stone is handed to the other player as prisoner. Since black started the game, white must end the game, an additional pass may be necessary.  


#### Liberties
Liberties are the spaces above, below, left and right of a stone or string of stones that are unoccupied by other stones. A string of stones are adjacent stones of the same color. All stones in a string share the same number of liberties. 
When the number of liberties is zero, the stone or string of stones is captured and are removed from the board. 

#### Ko
Ko is a pattern of stones that allows for a player to capture a stone but in the next move to have that stone be captured by the other player. For example, in each of the patterns below, 'O' could capture '@' at each of the '~' positions, but then be captured by '@' the following term. 

Ko patterns:
```
. . O @ . . 
. O @ ~ @ .
. . O @ . .
. . . . . . 
@ . . . . .
~ @ . . . .
@ O . . O @
O . . O @ ~
```
To prevent indefinite recapture, the second player is not allowed to recapture the first piece until making a move elsewhere. The first player then could choose to fill the ko or allow the second player to retake it.


#### Scoring 
Scoring is done after the game ends. 

There are two ways to earn points:
  - Each vacant point in a player's territory is counted as one point. 
  - Each captured stone (prisoner) is worth one point

Going first has an advantage, so to compensate, white gets a points added to their score. These additional points are called *komi* and are normally set to 7 points.
 
The player with the higher score wins. 

## Further Reading

