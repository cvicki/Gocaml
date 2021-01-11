open Command
open Game
open Util

let welcome_message = 
  {|
  ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■
  ■           WELCOME TO GOCAML           ■
  ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■

  Supported Moves
  - play <position>
    make sure the position is a single uppercase letter followed by a number 
    between 1 and the board size specified in the game file :)
  - quit
    for when you are done playing :(
  - save <filename>
    for when you are done playing but want to save your current progress
  - print
    to print the stones currently on the board
  - score
    to print the current score 
  - undo
    to undo the previous player's move
  - forfeit
    to forfeit the game :( 

  If you ever want to quit the terminal, press ^D (control D).
  |}

let exit_message = "We hope you enjoyed playing GOCaml and come back soon!"

let forfeit_message game = 
  match turn game with
  | Black -> "Player 1 has forfeit. \nPlayer 2 has won the game!" 
  | White -> "Player 2 has forfeit. \nPlayer 1 has won the game!" 

let board_size_message = 
  "Please choose from these board sizes: [5, 7, 9, 11, 13, 19]"

let score_str (a, b) = 
  "player 1: " ^ string_of_float(a) ^ "\nplayer 2: " ^ string_of_float(b)

(** [play game t] manages each turn, parsing input, and displaying helpful 
    information to the terminal. [t] is the UNIX time this move started. *)
let rec play game t0 = 
  let name = if turn game = White then "W" else "B" in
  print_string (name ^ " > ");
  let open ANSITerminal in
  let user_input = read_line () in 
  try 
    let t1 = time () in
    match parse game user_input with
    | Quit -> print_endline exit_message; exit 0
    | Pass -> play (step game None 0) t1
    | Print -> print_endline (string_of_board game); play game t0
    | Score -> print_endline (score_str (score game)); play game t0
    | Forfeit -> print_endline (forfeit_message game); exit 0
    | Play pos -> play (step game (Some (istone_pos pos)) (t1 - t0)) t1
    | Undo -> play (undo game) t0
    | Save s -> begin
        let exists = Sys.file_exists s in 
        if not exists then (to_json game s; print_endline exit_message; exit 0)
        else (print_endline "A file with this name already exists."; 
              print_endline "Please choose a different name."; play game t0)
      end
  with 
  | Empty -> 
    print_string [red] "You didn't type anything! Try again! \n"; 
    play game t0
  | Deformed -> 
    print_string [red] "That's not a valid command! \n";
    play game t0
  | GoOutOfBounds ->
    print_string [red] "The position is out of the game bounds \n"; 
    play game t0
  | StoneAlreadyExists ->
    print_string [red] "A stone already exists in that location. \n";
    play game t0
  | KoException -> 
    print_string [red]
      "You cannot place a stone here. The move causes a Ko violation. \n";
    play game t0
  | SelfCaptureException -> 
    print_string [red] 
      "You cannot play there. That would cause a Self Capture \n";
    play game t0
  | GameEndException -> 
    let p1_score = fst (score game) in 
    let p2_score = snd (score game) in 
    let winner = 
      if p1_score > p2_score then fst (names game) 
      else snd (names game) 
    in
    print_string [blue]
      ("Two playes have passed, the game is now over!\nThe winner is " 
       ^ winner ^ "\nPlayer1: " ^ string_of_float p1_score ^ "\nPlayer2: " 
       ^ string_of_float p2_score ^ "\n");
    exit 0

(** [main] prompts for the game to play, then starts it. *)
let main () = let open ANSITerminal in
  print_string [green] welcome_message;
  print_string [cyan] 
    "Would you like to play a [new] game or [load] saved game?";
  print_string [default] "\n> ";
  let rec init () = 
    match read_line () with
    | exception End_of_file -> ()
    | f ->
      try 
        play (Yojson.Basic.from_file f |> from_json) (time ()) 
      with 
      | Sys_error _ -> 
        print_string [red] "Please enter a valid GOCaml file.";
        print_string [default] "\n> ";
        init ()
  in
  let rec new_game () = 
    match read_line () with 
    | exception End_of_file -> ()
    | f -> 
      match  int_of_string_opt f with 
      | None -> 
        print_string [red] board_size_message;
        print_string [default] "\n> "; 
        new_game ()
      | Some i -> 
        if List.mem i [5; 7; 9; 11; 13; 19] then 
          let game_file = "games/" ^ string_of_int i ^ ".json" in 
          play (Yojson.Basic.from_file game_file |> from_json) (time ())
        else 
          (print_string [red] board_size_message;
           print_string [default] "\n> "; 
           new_game ())
  in 
  let rec new_load () =
    match read_line () with
    | exception End_of_file -> print_endline "EOF Exception"; ()
    | "new" -> 
      print_string [cyan] board_size_message;
      print_string [default] "\n> ";
      new_game ()
    | "load" -> 
      print_string [cyan] "Please enter GOcaml file name.";
      print_string [default] "\n> ";
      init ()
    | f -> 
      print_string [red] "Please choose 'new' or 'load'.";
      print_string [default] "\n> "; 
      new_load ()
  in 
  new_load ()

let () = main ()
