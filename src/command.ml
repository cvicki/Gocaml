open Game

exception Deformed

exception Empty

exception GoOutOfBounds

exception StoneAlreadyExists

type command = 
  | Pass
  | Play of string
  | Forfeit
  | Save of string
  | Quit
  | Print
  | Score 
  | Undo 

let istone_pos pos =
  try 
    let col = Char.code (String.get pos 0) - 65 in
    let row = int_of_string (Str.string_after pos 1) - 1
    in (col, row)
  with 
  | _ -> raise Deformed

(** [extract_command lst] rasies [Empty] if [lst] is empty and  [Deformed] if 
    the lst doesn't have a correct format. Returns a command otherwise. *)
let extract_command = function
  | h :: [] ->
    if h = "pass" then Pass
    else if h = "forfeit" then Forfeit
    else if h = "quit" then Quit 
    else if h = "print" then Print 
    else if h = "undo" then Undo 
    else if h = "score" then Score else raise Deformed
  | h :: t :: [] ->
    if h = "play" then Play t
    else if h = "save" then Save t else raise Deformed
  | [] -> raise Empty
  | _ -> raise Deformed

(** [valid_placement game cmd] makes sure that the stone position of the play 
    command is within the bounds of the board and is currently empty. 
    Raises: [GoOutOfBounds] if the desired stone location for the Play command 
    is not within the bounds of the board. *)
let valid_placement game cmd =
  match cmd with
  | Play str -> 
    let ipos = istone_pos str in
    if in_bounds game ipos then 
      if is_empty game ipos then cmd else raise StoneAlreadyExists 
    else raise GoOutOfBounds
  | Pass | Forfeit | Quit | Save _ | Score | Print | Undo -> cmd

(** [is_valid game lst] is the command extracted from [lst]. 
    Raises: [Deformed] if the command is invalid. *)
let is_valid game lst = 
  if List.length lst > 2 then raise Deformed;
  let cmd_out = extract_command lst |> valid_placement game 
  in cmd_out

let parse game str = 
  let cmd_in = 
    String.split_on_char '\032' str 
    |> List.map String.trim 
    |> List.filter (fun s -> s <> "")
  in
  is_valid game cmd_in 
