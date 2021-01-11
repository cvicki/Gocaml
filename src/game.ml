open Util
open Yojson.Basic.Util

exception KoException

exception SelfCaptureException

exception StoneAlreadyExistsException

exception TimeExpiredException

exception GameEndException

type stone = Black | White

(** [player] is the type representing a single player in a go game. *)
type player = {
  (** [id] is the name of the player *)
  id : string;
  (** [prisoners] is the prisoners captured by the player *)
  prisoners : (int * int * int) list;
  (** [byoyomi] is the number of byoyomi periods the player has left *)
  byoyomi : int;
  (** [game_time] is the amount of time remaining on the player's game clock. *)
  game_time : int;
}

(** [players] is the type representing a set of 2 players in a go game. *)
type players = {
  (** [p1] is the player with black stones *)
  p1 : player;
  (** [p2] is the player with white stones *)
  p2 : player;
}

(** [board] is the type presenting a single go board. *)
type board = {
  (** [size] is the nxn dimensions of a go board. *)
  size : int;
  (** [white] and [black] represent the column, row, and cur_stones in play 
      after the move, respectively. The column number is the index of the 
      character of the column in the alphabet. 
      Example:
      - 1, 1, 1 would be the stone placed B2, which becomes the only 1 stone on 
        the board. *)
  white : (int * int * int) list;
  black : (int * int * int) list
}

type config = {
  (** [byoyomi_period] is the length in seconds of each byo-yomi period. *)
  byoyomi_period : int;
  (** [komi] is the compensation awarded to the player who goes second, who is 
      placed at a measurable disadvantage. *)
  komi : float;
  (** [turn] is the player whose turn is currently is. *)
  turn : char;
}

type t = {
  players : players;
  board : board;
  config : config;
}

(** [player_stones lst] is the stones currently in [lst]. *)
let player_stones lst =
  let rec stones_r acc = function
    | [] -> List.rev acc
    | h :: t ->
      let col = h |> member "col" |> to_int in
      let row = h |> member "row" |> to_int in
      let cur_stones = h |> member "cur_stones" |> to_int in
      stones_r ((col, row, cur_stones) :: acc) t
  in stones_r [] lst

(** [to_player json] is the player record represented by a valid player. *)
let to_player json = 
  let prisoners = 
    json 
    |> member "prisoners" 
    |> to_list 
    |> player_stones 
  in {
    byoyomi = json |> member "byoyomi" |> to_int;
    game_time = json |> member "game_time" |> to_int;
    id = json |> member "id" |> to_string;
    prisoners = prisoners;
  }

(** [to_board json] is the board record represented by the valid json board. *)
let to_board json =
  {
    size = json |> member "size" |> to_int;
    white = json |> member "white" |> to_list |> player_stones;
    black = json |> member "black" |> to_list |> player_stones;
  }

(** [to_config j] is the [config] record corresponding to the config [j]. *)
let to_config json = 
  let turn = 
    if json |> member "turn" |> to_string = "b" then 'b' else 'w' 
  in
  {
    byoyomi_period = json |> member "byoyomi_period" |> to_int;
    komi = json |> member "komi" |> to_float;
    turn = turn;
  }

let from_json json =
  let players = json |> member "players" in
  let p1 = players |> member "p1" |> to_player in
  let p2 = players |> member "p2" |> to_player in
  let board = json |> member "board" |> to_board in
  let config = json |> member "config" |> to_config in
  {
    players = {
      p1 = p1;
      p2 = p2;
    };
    board = board;
    config = config;
  }

(** [from_move m] is the json representation of a single move as specified by 
    the column, row, and move number of a given stone. *)
let from_move col row mov = 
  Printf.sprintf 
    {|{      
      "col" : %d,
      "row" : %d,
      "cur_stones" : %d
    }|} col row mov

(** [from_player p name] is the json representation of a [player] record with 
    key [name]. *)
let from_player p name =
  let moves lst = 
    List.map (fun (c, r, m) -> from_move c r m) lst 
    |> string_of_list (fun id -> id)
  in
  Printf.sprintf 
    {|"%s" : {
        "byoyomi" : %d,
        "game_time" : %d,
        "id" : "%s",
        "prisoners" : %s
    }|} name p.byoyomi p.game_time p.id (moves p.prisoners)

(** [from_players ps] is the json representation of a [players] record. *)
let from_players ps = 
  Printf.sprintf 
    {|"players" : {
      %s,
      %s
    }|} (from_player ps.p1 "p1") (from_player ps.p2 "p2")

(** [from_board b] is the json representation of a [board] record [b]. *)
let from_board b = 
  let moves lst = 
    List.map (fun (c, r ,m) -> from_move c r m) lst 
    |> string_of_list (fun id -> id)
  in
  Printf.sprintf 
    {|"board" : {
        "size" : %d,
        "white" : %s,
        "black" : %s
    }|} b.size (moves b.white) (moves b.black)

(** [from_config c] is the json representation of a [config] record [c]. *)
let from_config c = 
  Printf.sprintf 
    {|"config" : {
        "byoyomi_period" : %d,
        "komi" : %f,
        "turn" : "%c"
    }|} c.byoyomi_period c.komi c.turn

let to_json t out_file =
  let content = 
    Printf.sprintf 
      {|{ 
        %s,
        %s,
        %s
      }|} (from_players t.players) (from_board t.board) (from_config t.config)
  in 
  let out_file' = 
    if String.sub out_file (String.length out_file - 5) 5 <> ".json" 
    then out_file ^ ".json" else out_file in 
  let oc = open_out out_file' in
  Printf.fprintf oc "%s\n" content;
  close_out oc

(** [positions lst] is the first and second value in each element of [lst]. 
    Given a stone color of a board, this is the column and row of each stone of 
    that color. *)
let positions lst = 
  let rec pos_tr acc = function
    | (r, c, m) :: t -> pos_tr ((r, c) :: acc) t
    | [] -> acc
  in pos_tr [] lst

let stones t = function
  | Black -> positions t.board.black
  | White -> positions t.board.white

let adjacent = [(1, 0); (0, 1); (-1, 0); (0, -1)]

let in_bounds t (col,row) = 
  let max_size = t.board.size in
  col >= 0 && col < max_size 
  &&
  row >= 0 && row < max_size

let bounds t = 
  t.board.size

let is_empty t pos = 
  if in_bounds t pos then 
    not (List.mem pos (stones t Black) || List.mem pos (stones t White))
  else false

(** [c_adjacent pos] are the coordinates of all the positions adjacent to 
    [pos]. *)
let c_adjacent pos =
  List.map (fun a -> combine_t (+) pos a) adjacent

(** [pos_stones t pos] is the list of stones in [t] that are the same color as 
    the stone at [pos]. *)
let pos_stones t pos = 
  if List.mem pos (stones t Black) then stones t Black 
  else if List.mem pos (stones t White) then stones t White 
  else []

(** [stone_color t pos] is tjhe color of the stone at position [pos] in game 
    [t]. *)
let stone_color t pos = 
  if List.mem pos (stones t Black) then Some Black 
  else if List.mem pos (stones t White) then Some White 
  else None

(** [group t pos] is the group of stones of the same color as the stone at 
    [pos] that are adjacently-connected to [pos]. *)
let group t pos =
  let stones = pos_stones t pos in
  let stack = ref [pos] in
  let visited = ref [] in
  (** [find_same_adj pos] finds all stones of the same color as the one at [pos]
      that have not been visited or currently in [stack], and adds (by mutating)
      these new stones to the [stack]. *)
  let find_same_adj pos =
    let boundary = 
      c_adjacent pos |> 
      List.filter 
        (fun pos -> 
           List.mem pos stones 
           && 
           not (List.mem pos !visited)
           &&
           not (List.mem pos !stack)) 
    in
    visited := pos :: !visited;
    stack := !stack @ boundary
  in
  (** [connected_r pos] is the group of stones connected to [pos]. *)
  let rec connected_r pos =
    while !stack != [] do
      find_same_adj (List.hd !stack); (* Note: safe, as stack is not empty. *)
      stack := List.tl !stack;
    done
  in connected_r pos; !visited

let liberties t pos =
  let connected = group t pos in
  let all_adjacent = 
    List.map (fun s -> c_adjacent s) connected 
    |> List.flatten 
    |> List.sort_uniq compare in
  let all_liberties = 
    List.map (fun c -> if is_empty t c then 1 else 0) all_adjacent 
  in List.fold_left (fun acc v -> acc + v) 0 all_liberties

let turn t =
  if t.config.turn = 'b' then Black else White

let names t = 
  (t.players.p1.id, t.players.p2.id)

(** [cur_player t] is the player record corresponding to whose turn it currently
    is. *)
let cur_player t = 
  match turn t with
  | Black -> t.players.p1
  | White -> t.players.p2

(** [n_stones] is the number of stones currently on the board in [t]. *)
let n_stones t =
  let n_moves = function
    | Black -> max_triple3 t.board.white
    | White -> max_triple3 t.board.black
  in let _, _, max_moves = max_triple3 [(n_moves Black); (n_moves White)]
  in max_moves

let last_stone t = 
  let s = match turn t with
    | White -> t.board.black 
    | Black -> t.board.white
  in 
  match max_triple3 s with
  | (c, r, _) -> (c, r)

(** [deduct_time t time] is the game with the proper time parameters after 
    the player who just went spent [time] seconds on their move. *)
let deduct_time t time = 
  let p = cur_player t in
  let t_new = 
    if time <= p.game_time then (p.byoyomi, p.game_time - time)
    else let periods_used = (time - p.game_time) / t.config.byoyomi_period in
      if p.byoyomi > periods_used then (p.byoyomi - periods_used, 0)
      else raise TimeExpiredException
  in t_new

(** [remove_stones t s] removes all the stones in [s] from the board in [t]. *)
let remove_stones t s =
  let remove_cr t (c, r) = 
    let b = match turn t with
      | Black -> t.board.black 
      | White -> t.board.white
    in
    List.filter (fun (c', r' , _) -> not (c' = c && r' = r)) b
  in
  let rec remove_stone t = function
    | (c, r) :: tail -> begin
        let p1', p2', board' = match turn t with
          | White -> 
            let p1_pris' = (c, r, n_stones t) :: t.players.p1.prisoners in
            {t.players.p1 with prisoners = p1_pris'},
            t.players.p2,
            {t.board with white = remove_cr t (c,r)}
          | Black -> 
            let p2_pris' = (c, r, n_stones t) :: t.players.p2.prisoners in
            t.players.p1,
            {t.players.p2 with prisoners = p2_pris'},
            {t.board with black = remove_cr t (c,r)}
        in
        let players' = {p1 = p1'; p2 = p2'} in
        let t' = {t with board = board'; players = players'} 
        in remove_stone t' tail
      end
    | [] -> t
  in remove_stone t s

(** [remove_prisoners t pos] checks the board in [t] and determines if any 
    stones were captured from the stone placed at position [pos]. *)
let remove_prisoners t pos =
  let adj = List.filter 
      (fun p -> 
         in_bounds t p 
         && 
         stone_color t p = Some (turn t)
      ) (c_adjacent pos) in
  let groups = List.map (fun a -> (liberties t a, group t a)) adj in
  List.fold_left 
    (fun acc (libs, stones) -> 
       if libs = 0 then remove_stones t stones else acc
    ) t groups

(** [opposite_adj t p] finds all the valid, non-empty adjacent positions to the 
    group formed at position [p] (i.e. are the opposite color). *)
let opposite_adj t pos = 
  List.fold_left (fun acc pos -> acc @ c_adjacent pos) [] (group t pos)
  |> List.filter (fun p -> in_bounds t p && not (is_empty t p))

let self_sacrifice t pos = 
  let opp_adj = opposite_adj t pos in
  let max_adj_liberties = 
    match list_max (List.map (fun p -> liberties t p) opp_adj) with
    | Some v -> v
    | None -> 0
  in
  if List.fold_left (fun acc pos -> acc + liberties t pos) 0 (group t pos) = 0 
     && 
     max_adj_liberties > 0
  then raise SelfCaptureException 
  else ()

let handicap t lst = 
  let n, placements = List.fold_left (fun (n, acc) (c,r) -> 
      (n + 1, (c, r, n) :: acc)) (1, []) lst in
  let board' = {t.board with black = placements} in
  let config' = {t.config with turn = 'w'} 
  in {t with board = board'; config = config'}

let new_player t time =
  let (byoyomi, game_time) = deduct_time t time in
  let p = cur_player t in {p with byoyomi = byoyomi; game_time = game_time;}

let new_players t time move = 
  let placement = match move with
    | None -> `Pass
    | Some pos -> `Place
  in
  let pn = match placement, turn t with 
    | `Pass, Black -> 
      {t.players.p2 with prisoners = 
                           (-1, -1, n_stones t) :: t.players.p2.prisoners}
    | `Pass, White -> 
      {t.players.p1 with prisoners = 
                           (-1, -1, n_stones t) :: t.players.p1.prisoners}
    | `Place, Black -> new_player t time
    | `Place, White -> new_player t time
  in 
  match move, turn t with
  | None, Black | Some _, White -> {t.players with p2 = pn}
  | None, White | Some _, Black -> {t.players with p1 = pn}

(** [lst_head_h lst] returns the head of a list if it exists. Otherwise it 
    returns a tuple that will not match with any player move.
    This method is used to get the head of the prisoners list (accounts for the
    empty list case) *)
let lst_head_h lst = 
  match lst with 
  | [] -> (-1, -1, -1)
  | h :: t -> h

let ko t (c,r) = 
  let num_stones  = n_stones t in 
  let p = if (turn t) = Black then lst_head_h t.players.p2.prisoners else
      lst_head_h t.players.p1.prisoners in
  let p_curr_stone = match p with 
    | (_,_,l) -> l
  in 
  let p_pos = match p with 
    | (x,y,_) -> (x,y)
  in 
  (c,r) = p_pos && (num_stones = p_curr_stone) 

(** [new_board t m] is the updated board for [t] after move [m]. *)
let new_board t (c,r) = 
  if not (is_empty t (c,r)) then raise StoneAlreadyExistsException
  else if ko t (c,r) then raise KoException 
  else 
    let move' = (c, r, 1 + n_stones t) in
    match (turn t) with
    | Black -> {t.board with black = move' :: t.board.black}
    | White -> {t.board with white = move' :: t.board.white}

(** [new_config t] is the updated configuration for [t]. This updates the 
    player's turn. *)
let new_config t = 
  let turn' = if (turn t) = Black then 'w' else 'b' in
  {t.config with turn = turn'}

(** [game_end t] checks to see if the last prisoner in the players turn was due 
    to a 'pass' command. This is used in determining the GameEndException *)
let game_end t = 
  let num_stones  = n_stones t in 
  let pris = 
    if (turn t) = Black then lst_head_h t.players.p1.prisoners 
    else lst_head_h t.players.p2.prisoners 
  in
  let p_curr_stone = match pris with 
    | (_ ,_ , l) -> l
  in 
  let p_pos = match pris with 
    | (x, y, _) -> (x, y)
  in 
  let pris1_len = List.length t.players.p1.prisoners in
  let pris2_len = List.length t.players.p2.prisoners in
  num_stones = p_curr_stone && (-1, -1) = p_pos 
  && not (0 = pris1_len || 0 = pris2_len)

let step t move time = 
  let board' = match move with 
    | None -> t.board
    | Some pos -> new_board t pos 
  in
  let players' = new_players t time move in
  let config' = new_config t in
  let t' = {
    players = players';
    board = board';
    config = config'
  } in
  match move with
  | None -> if game_end t' then raise GameEndException else t'
  | Some p -> 
    let t'' = remove_prisoners t' p in
    self_sacrifice t'' p; t''

let star_locations t = 
  let n1 = 
    Float.floor ((float_of_int (bounds t) -. 1.0) /. 12.0) +. 2.0 
    |> int_of_float in
  let n2 = bounds t - (n1 + 1) in
  let n3 = 
    float_of_int (bounds t) /. 2.0 
    |> Float.floor 
    |> int_of_float 
  in (n1, n2, n3)

let handicap_c t h = 
  let h_positions = ref [] in
  let add lst = 
    h_positions := !h_positions @ lst 
  in
  let n1, n2, n3 = star_locations t in
  if bounds t >= 7 then 
    (if h >= 2 then add [(n1, n2); (n2, n1)];
     if h = 3 then add [(n2, n2)];
     if h >= 4 then add [(n1, n1); (n2, n2)]);
  if bounds t >= 9 then
    (if h >= 5 && h mod 2 = 1 then add [(n3, n3)];
     if h >= 6 then add [(n1, n3); (n2, n3)];
     if h >= 8 then add [(n3, n1); (n3, n2)];);
  !h_positions

(**[undo_white_helper t] is the game at the start of white's last turn in [t] *)
let undo_white_helper t =
  let (c, r, prev_pris) = List.hd t.players.p2.prisoners in 
  let prisoners = 
    List.filter (fun (c,r,a) -> a <> prev_pris) t.players.p2.prisoners in 
  let freed = 
    List.filter (fun (c,r,a) -> a = prev_pris) t.players.p2.prisoners in 
  let p2' = {t.players.p2 with prisoners = prisoners} in 
  let players' = {t.players with p2 = p2'} in 
  let board' = 
    (* {t.board with white = List.tl t.board.white}  *)
    {
      size = t.board.size;
      black = freed @ t.board.black;
      white = List.tl t.board.white
    }
  in 
  (* let board'' = {board' with black = freed @ t.board.black} in  *)
  let config' = new_config t in 
  {
    players = players';
    board = board';
    config = config'
  }

(**[undo_black_helper t] is the game at the start of black's last turn in [t]*)
let undo_black_helper t =
  let c, r, prev_pris = List.hd t.players.p1.prisoners in 
  let prisoners = 
    List.filter (fun (c,r,a) -> a <> prev_pris) t.players.p1.prisoners in 
  let freed = 
    List.filter (fun (c,r,a) -> a = prev_pris) t.players.p1.prisoners in
  let p1' = {t.players.p1 with prisoners = prisoners} in 
  let players' = {t.players with p1 = p1'} in 
  let board' = 
    {
      size = t.board.size;
      black = List.tl t.board.black;
      white = freed @ t.board.white
    }
  in 
  let config' = new_config t in 
  {
    players = players';
    board = board';
    config = config'
  }

(**[black_helper] is the game at the start of black's last turn in [t],
    handles the case where it is black's first turn *)
let black_helper t = 
  if List.length t.board.white = 0 then 
    (print_endline "cannot undo first move"; t )
  else 
    let board' = {t.board with white = List.tl t.board.white} in
    let config' = new_config t in 
    { players = t.players; board = board'; config = config'}

(**[get_curr_stone lst] is the current number of stones on the board according 
    to [lst] *)
let get_curr_stone = function
  | [] -> None 
  | (c,r,curr) :: t -> Some curr 

(**[captured t] is true if stones were captured last turn in game [t]*)
let captured t = 
  let w_board = get_curr_stone t.board.white in 
  let b_board = get_curr_stone t.board.black in 
  let w_pris = get_curr_stone t.players.p1.prisoners in 
  let b_pris = get_curr_stone t.players.p2.prisoners in 
  let compare_curr prev_p curr_b = 
    if curr_b = prev_p then true else false 
  in 
  match turn t with 
  | Black -> compare_curr b_pris w_board
  | White -> compare_curr w_pris b_board 

let undo t  = 
  match turn t, captured t with 
  | White, false ->
    let board' = {t.board with black = List.tl t.board.black} in
    let config' = new_config t in 
    { players = t.players; board = board'; config = config'}
  | White, true -> undo_black_helper t 
  | Black, false -> black_helper t 
  | Black, true ->if List.length t.board.white = 0 then black_helper t 
    else undo_white_helper t 

(** [create_labels d a c acc] if [alph] is true it is the alphabetic labels
      for a board of size [dim], else, it is the numberic labels 
      Ex. [create_labels 3 false 0 []] is [["0";"1";"2"]] *)
let rec create_labels dim alph counter acc= 
  if counter = dim then List.rev acc 
  else 
    match alph with 
    | true -> let v = Char.escaped (Char.chr (counter + 64)) in 
      create_labels dim alph (counter + 1) (v :: acc)
    | false ->if counter >= 10 then let v = string_of_int counter in 
        create_labels dim alph (counter + 1) (v :: acc)
      else let v = " " ^ string_of_int counter   in 
        create_labels dim alph (counter + 1) (v :: acc)

(** [full_board t] creates an nxn matrix consisting of dots to represent an 
    empty Go board of size n. *)
let full_board t = 
  let dim = t.board.size + 1 in 
  let grid = Array.make_matrix dim dim "⋅" in
  let rec add_labels (x,y) alph = function 
    | h :: t -> grid.(x).(y) <- h; if alph then add_labels (x,y + 1) alph t
      else  add_labels (x + 1,y) alph t
    | [] -> () in 
  let rec add_stones rep = function 
    | (c, r, _) :: t -> grid.(r+ 1).(c+1) <- rep; add_stones rep t
    | [] -> ()
  in add_stones "W" t.board.white; add_stones "B" t.board.black; 
  add_labels (0,0) true (create_labels dim true 0 []); 
  add_labels (0,0) false (create_labels dim false 0 []);
  grid.(0).(0) <- "  "; grid

let string_of_string_array arr =  
  String.concat " " (Array.to_list arr)

let string_of_string_string_array arr =
  let lst = Array.map string_of_string_array arr |> Array.to_list in
  String.concat "\n" lst

let string_of_board t =
  string_of_string_string_array (full_board t)

(* MARK: - Scoring  Implementation *)
type intersection = WhiteT | BlackT | Neutral | WhiteS | BlackS | Empty

(** [init_grid t] is a nxn matrix corresponding to [t] with White stones marked 
    as [WhiteS], Black as [BlackS], and all others as [Empty]. *)
let init_grid t = 
  let dim = t.board.size in
  let grid = Array.make_matrix dim dim Empty in
  (** [populate_grid s xs] marks all the stones in [s] to [xs]. *)
  let populate_grid s xsection = 
    List.iter (fun (c, r) -> grid.(r).(c) <- xsection) s
  in
  populate_grid (stones t Black) BlackS; populate_grid (stones t White) WhiteS;
  grid

(** [explore t grid pos] is the positions that are currently empty and reachable
    from [pos] along with the positions on the boundary of this region. *)
let explore t grid pos =
  let stack = ref [pos] in
  let visited = ref [] in
  let boundary = ref [] in
  let traverse pos = 
    let adj = 
      List.filter (fun (c, r) -> in_bounds t (c, r)) (c_adjacent pos) in
    let stackable = 
      List.filter 
        (fun (c, r) -> 
           grid.(r).(c) = Empty
           && 
           (not (List.mem (c, r) !visited))
           &&
           (not (List.mem (c, r) !stack))
        ) adj in
    let unstackable = 
      List.filter (fun (c, r) -> grid.(r).(c) != Empty) adj in
    boundary := !boundary @ unstackable;
    stack := !stack @ stackable;
    visited := pos :: !visited;
  in 
  while !stack != [] do
    match !stack with
    | h :: t -> stack := t; traverse h; 
    | [] -> ()
  done; (!visited, !boundary)

let fill_grid t = 
  let grid = init_grid t in
  (** [has_x lst xs] is whether [xs] is contained within [lst]. *)
  let has_x lst xsection = 
    let filtered = List.filter (fun (c, r) -> grid.(r).(c) = xsection) lst 
    in List.length filtered > 0
  in
  (** [mark lst xs] is [grid] with all positions specified in [lst] changed to 
      [xs]. *)
  let mark lst xsection = 
    List.iter 
      (fun (c, r) -> 
         if (grid.(r).(c) != BlackS && grid.(r).(c) != WhiteS) 
         then grid.(r).(c) <- xsection
      ) lst
  in
  for r = 0 to Array.length grid - 1 do
    for c = 0 to Array.length grid.(r) - 1 do 
      let should_explore = grid.(r).(c) = Empty in
      if should_explore then 
        let visited, boundary = explore t grid (c, r) in
        let has_black = has_x boundary BlackS in
        let has_white = has_x boundary WhiteS in
        let has_neutral = has_x boundary Neutral in
        if (has_black && has_white) || has_neutral then mark visited Neutral
        else if has_black && not has_white then mark visited BlackT
        else if not has_black && has_white then mark visited WhiteT
        else mark visited Neutral
    done
  done; grid

let score t =
  let grid = fill_grid t in
  let flattened = 
    Array.map (fun x -> Array.to_list x) grid |> Array.to_list |> List.flatten 
  in
  let black = List.filter (fun e -> e = BlackT) flattened |> List.length in
  let white = List.filter (fun e -> e = WhiteT) flattened |> List.length in
  let b_prisoners = List.length t.players.p1.prisoners in
  let w_prisoners = List.length t.players.p2.prisoners in
  let komi = t.config.komi 
  in (float_of_int (black + b_prisoners), 
      komi +. float_of_int (white + w_prisoners))

let game_message t = 
  let name1, name2 = names t in 
  let time1, time2 = 
    t.players.p1.game_time |> string_of_int, 
    t.players.p2.game_time |> string_of_int in
  let byo1, byo2 = 
    t.players.p1.byoyomi |> string_of_int,
    t.players.p2.byoyomi |> string_of_int in
  let stones1, stones2 = 
    List.length t.board.black |> string_of_int, 
    List.length t.board.white |> string_of_int in
  let pris1, pris2 = 
    List.length t.players.p1.prisoners |> string_of_int, 
    List.length t.players.p2.prisoners |> string_of_int in
  let komi = t.config.komi in
  let score1, score2 = score t in
  let terr1, terr2 =   
    (score1 -. (float_of_string pris1)) |> string_of_float,
    (score2 -. (float_of_string pris2) -. komi) |> string_of_float 
  in
  (["Player"; "Name"; "Stone Color"; "Seconds"; "Stones"; "Byoyomi"; "Komi"; 
    "Territory"; "Prisoners"; "Score"], 
   ["Player 1"; name1; "Black"; time1; stones1; byo1; "0"; terr1; pris1; 
    string_of_float score1], 
   ["Player 2"; name2; "White"; time2; stones2; byo2; string_of_float komi; 
    terr2; pris2; string_of_float score2])
[@@coverage off]