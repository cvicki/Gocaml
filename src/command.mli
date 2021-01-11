
(** Command interprets user input when playing the terminal version of 
    GOcaml. *)

(** [Deformed] is raised when a player's input is not in the correct format. *)
exception Deformed

(** [Empty] is raised when the players input is empty. *)
exception Empty

(** [GoOutOfBounds] is raised when the stones being places exceeds the valid 
    bounds of the board. *)
exception GoOutOfBounds

(** [StoneAlreadyExists] is raised when a player attempts to place a 
    stone on top of an existing stone. *)
exception StoneAlreadyExists

(** [command] is a player's move in a game. They have the option to 
    - Pass
    - Play a stone at a coordinate marked by a character and an integer 
      between 1 and the board size.
        Example: "A1" places a stone in the top left corner. 
    - Forfeit
    - Save the current game to the file specified 
    - Quit will quit the game application 
    - Print will print the current contents of the board 
    - Score prints the current score of each player
    - Undo restores the game to the beginning of the previous player's turn
        excluding time*)
type command = 
  | Pass
  | Play of string
  | Forfeit
  | Save of string
  | Quit
  | Print
  | Score 
  | Undo 

(** [istone_pos pos] is the integer position of [pos].
    Requires: [pos] is a single capitalized alphabetic character between A and 
    the board_size - 1th character in the alphabet followed by an integer in the
    interval [1, board_size]. *)
val istone_pos : string -> (int * int)

(** [parse game str] parses a player's input into a [command]. The verb must be
    all lowercase and the string following the verb is must adhere to the above 
    specifications for [command].

    Raises: [Deformed] if the command is not in a valid format. 
    Raises: [Empty] if [str] does not contain alphanumeric characters. 
    Raises: [StoneAlreadyExists] if the stone placement is on top of an 
      existing stone.
    Raises: [GoOutOfBounds] if the stone placement exceeds the bounds of the 
      board.

    Requires: [str] contains alphanumerics and spaces. *)
val parse : Game.t -> string -> command
