
(** Game contains the logic required for loading, saving, and playing a GOcaml 
    game. It represents the internal game state and does not provide any 
    user-facing interactive capabilities.  *)

(** [t] is the abstract type of values representing go games. *)
type t

(** [stone] is the type representing the black or white stones *)
type stone = Black | White

(** [intersection] is the type representing how a single intersection may be 
    viewed when scoring a Go game. 
    Example: 
      White stone and white territory correspond to [WhiteS] and [WhiteT], 
        respectively. *)
type intersection = WhiteT | BlackT | Neutral | WhiteS | BlackS | Empty

(** [KoException] is raised when the configuration of a Go board is repeated 
    twice in a single game. *)
exception KoException

(** [SelfCaptureException] is raised when a player attempt to place a stone in a
    position that is immediately captured. *)
exception SelfCaptureException

(** [StoneAlreadyExists] is raised when a player attempts to place a 
    stone on top of an existing stone. *)
exception StoneAlreadyExistsException

(** [TimeExpiredException] is raised when a player has exhausted their entire 
    game clock. *)
exception TimeExpiredException

(** [GameEndException] is raised when two players consecutively pass which 
    signals the game is over *)
exception GameEndException

(** [from_json json] is the Go game that [json] represents.
    Requires: [json] is a valid json Go representation. *)
val from_json : Yojson.Basic.t -> t

(** [to_json go str] writes the contents of [go] into a valid json.
    Does not overwrite pre-existing files.. *)
val to_json : t -> string -> unit

(** [in_bounds t (c,r)] is whether the stone at column [c] and row [r] is 
    within the bounds of the board. *)
val in_bounds : t -> (int * int) -> bool

(** [bounds t] is the nxn size of the board in game [t]. *)
val bounds : t -> int

(** [is_empty t pos] is whether there is no stone current at position [pos]
    in the [t]. *)
val is_empty : t -> (int * int) -> bool

(** [score t] is the current score of the game using the territory scoring. The
    first value in the tuple represents the score of the black stones and the 
    second value, the white stones. *)
val score : t -> (float * float)

(** [liberties t pos] is the number of liberties in the group at input [pos]. 
    Requires: a stone is currently placed at [pos]. *)
val liberties : t -> (int * int) -> int

(** [ko t str] is whether an input [str] would violate ko. *)
val ko : t -> (int * int) -> bool

(** [stones t s] is the col and row of every stone on the board of type 
    [stone]. *)
val stones : t -> stone -> (int * int) list

(** [turn t] is the stone of the player whose turn it currently is. *)
val turn : t -> stone

(** [names t] is the pair of player names, with the first value playing with 
    black stones and the second with white.  *)
val names : t -> (string * string)

(** [star_locations t] are the three possible locations for a star point on a Go 
    board. Star points act as a reference for Go boards and mark where handicap 
    stones, if any, are placed. *)
val star_locations : t -> int * int * int

(** [handicap_c t h] is the positions of [h] handicap stones on the board in 
    [t]. *)
val handicap_c : t -> int -> (int * int) list

(** [handicap t lst] places the handicap stones specified in [lst] on the board. 
    It then becomes white's turn. *)
val handicap : t -> (int * int) list -> t

(** [last_stone t] is the column and row, respecively, of the last stone placed 
    on the board in [t]. 
    Returns: (-1, -1) if there are no stones on the board. *)
val last_stone : t -> int * int

(** [step t move time] is the new game after the current player places a stone 
    on [move] and spends [time] seconds doing so. *)
val step : t -> (int * int) option -> int -> t

(** [string_of_board t] is the string representation of a board. *)
val string_of_board : t -> string

(** [fill_grid t] is the board in [t] with black, neutral, and blwhiteack 
    territory mark accordingly, as well as black and white stones. *)
val fill_grid : t -> intersection array array

(** [undo t] reverts to the game at beginning of the previous
    player's turn excluding time.  *)
val undo : t -> t 

(** [game_message t] describes all the relevant information about [t] 
    corresponding to the stat name, player 1 value, and player 2 value. *)
val game_message : t -> (string list * string list * string list)
