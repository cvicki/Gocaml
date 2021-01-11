open Game
open Graphics
open Util

(** [window_size] is the desired nxn dimension of the window which will display 
    the Go board. *)
let window_size = 800

(** [background_color] is the default background color for the graphical user 
    interface. *)
let background_color = rgb 235 195 120

let info_message = [
  "Welcome to GOcaml! This is implemented purely in OCaml,";
  "the best functional programming language! Here are some";
  "helpful shortcuts and commands to get started:"; "";
  " - 'P': to pass";
  " - 'S': to score the board";
  " - 'U': to undo the last move";
  " - 'I': to see information about the game";
  " - 'Q': to quit the GUI";
  " - 'I' then 'S': to save your current game";
  " - 'B': to navigation to a previous page when setting up";
  "   the game"; "";
  "Have fun playing GOcaml!";
  "- Banpreet, Cameron, and Vicki";
]

(** [axis] represents the columns [X] and rows [Y] of the Go board. This is
    necessary becuase the Graphics module considers (0,0) to be in the bottom 
    left, but our implementation has (0,0) in the top left, meaning only the Y 
    must be inverted. *)
type axis = X | Y

(** [display] represents the type of the contents currently being shown on the 
    window. 
    Main: the home page, allowing users to play the game or read info
    GOcamlInfo: the information about the Game in general
    LoadGame: where the user can choose to play a new or existing game
    BoardSize: where the user selects the board size for a new game
    Handicap: where the user selects the handicap for a new game
    FileLoad: where a user inputs a game file to load as an existing game
    Board: the current game state
    ScoredBoard: the current game state with each intersection scored
    Info: the info about the current game *)
type display = 
  | Main
  | GOcamlInfo
  | LoadGame 
  | BoardSize
  | Handicap
  | FileLoad
  | Board 
  | ScoredBoard 
  | Info 
  | GameOver

type board_dimensions = {
  (** [board_size] is the nxn dimension of the board. *)
  mutable board_size : int;
  (** [length] is the height/width of the window. The window is a square, so we 
      avoid just using height or width to avoid ambiguity. *)
  mutable length : int;
  (** [radius] is the radius of stones placed on the board. *)
  mutable radius : int;
  (** [spacing] is the pixel distance between each row and column. *)
  mutable spacing : int;
}

let b_dims = ref {
    board_size = 0;
    length = 0;
    radius = 0;
    spacing = 0;
  }

(** [setup_background color] changes the background color of the window to 
    [color]. *)
let setup_background color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color foreground

(** [font_size s] changes the font size to [s] for all text drawn on the GUI 
    after this function call. *)
let font_size size = 
  let size' = string_of_int size in
  set_font ("-*-Helvetica-medium-r-normal--" ^ size' ^ "-*-*-*-*-*-iso8859-1")

(** [index p axis] is the line on axis [axis] that position [p] is closest to. 
    On the Go board, lines are indexed top to bottom ([Y] axis), and left to 
    right ([X] axis). *)
let index pos axis = 
  let pos' = float_of_int (pos - 2 * !b_dims.spacing) in
  let spacing = float_of_int !b_dims.spacing in
  let unadjusted_index = (pos' /. spacing) |> Float.round |> int_of_float in
  match axis with
  | X -> unadjusted_index
  | Y -> !b_dims.board_size - unadjusted_index - 1

(** [coordinate_ c r] is the window coordinate corresponding to column [c] and 
    row [r] on a Go board. *)
let coordinate c r = 
  let coordinate_of_index idx axis = 
    let loc = 
      if idx < 0 then 0
      else if idx > !b_dims.board_size - 1 then !b_dims.board_size - 1
      else idx 
    in match axis with
    | X -> (loc + 2) * !b_dims.spacing
    | Y -> !b_dims.length - (loc + 2) * !b_dims.spacing
  in (coordinate_of_index c X, coordinate_of_index r Y)

(** [draw_stone x y r c] draws a circle at coordiante [(x, y)], radius [r],
    and color [c]. *)
let draw_stone x y r c = 
  set_color c;
  fill_circle x y r

(** [draw_stone_cr c,r radius color] draws a stone in the [c+1]th column and 
    [r+1]th row with color [color] with radius [radius]. *)
let draw_stone_cr (c,r) radius color = 
  let x, y = coordinate c r in
  draw_stone x y radius color

(** [draw_square_cr] draws a square over the [c+1]th column and [r+1]th row with
    a width and height equal to [length]. *)
let draw_square_cr (c,r) length = 
  set_color black;
  let x, y = coordinate c r in
  draw_rect (x - length / 2) (y - length / 2) length length 

(** [draw_ring x y c1 c2] draws a ring at coordiante [(x, y)] with inner color 
    [c1] and outer color [c2]. *)
let draw_ring x y c1 c2 = 
  let r1 = !b_dims.radius / 2 in
  let r2 = r1 + 3 in
  draw_stone x y r2 c2;
  draw_stone x y r1 c1

(** [prev_ring game] draws or removes a ring on the last placed stone. *)
let prev_ring game style =
  let c, r = last_stone game in
  let x, y = coordinate c r in
  match style with
  | `Remove -> begin
      if c >= 0 && r >= 0 then 
        match turn game with
        | Black -> draw_stone x y !b_dims.radius white
        | White -> draw_stone x y !b_dims.radius black
      else ()
    end
  | `Draw -> begin 
      match turn game with
      | Black -> draw_ring x y white black
      | White -> draw_ring x y black white
    end

(** [setup_title] draws the GOCAML logo on the current window *)
let setup_title () = 
  font_size 72;
  set_color white;
  moveto 250 533; 
  draw_string "GOCAML"

(** [setup_main_buttons b1 b2] creates 2 pseudo-buttons with the labels [b1] 
    and [b2]. *)
let setup_main_buttons button1 button2 =
  set_color black; 
  fill_rect 215 265 150 60;
  fill_rect 435 265 150 60;
  font_size 32; 
  set_color white; 
  moveto 250 277; 
  draw_string button1;
  moveto 470 277; 
  draw_string button2

(** [main_button_press e] determines which button, if any, was clicked according
    to [event] and returns whether the left or right pseudo-button was tapped 
    as created in [setup_main_buttons]. *)
let main_button_press event = 
  let x, y = event.mouse_x, event.mouse_y in
  let y_fit = y > 265 && y < 325 in
  let x1_fit = x > 215 && x < 365 in
  let x2_fit = x > 435 && x < 585 in
  if y_fit && x1_fit then `Left
  else if y_fit && x2_fit then `Right
  else `None

(** [setup_size_buttons] is the 5 buttons that allow a user to select a new 
    GOcaml game from the default board sizes. *)
let setup_size_buttons () = 
  font_size 32;
  let sizes = Array.of_list ["7"; "9"; "11"; "13"; "19"] in
  for i = 0 to Array.length sizes - 1 do
    set_color black;
    fill_rect (40 + 160 * i) 300 80 60;
    set_color white;
    moveto (160 * i + 65) 312;
    draw_string sizes.(i)
  done

(** [size_button_press event] is the size of the board corresponding to the 
    button pressed as initialized in [setup_size_buttons]. *)
let size_button_press event = 
  let x, y = event.mouse_x, event.mouse_y in
  let size_map = [(0, 7); (1, 9); (2, 11); (3, 13); (4, 19)] in
  let y_fit = y > 300 && y < 360 in
  let x_fit = (x - 40) mod 160 < 80 in
  let idx = x / 160 in
  if y_fit && x_fit 
  then Some (List.assoc idx size_map) 
  else None

(** [setup_message fs m] displays the message [m] in the center of the current 
    window with font size [fs]. *)
let setup_message ?fs:(fs = 24) message = 
  font_size fs;
  set_color black;
  moveto (window_size / 2 - 250) (window_size / 2 + 20);
  draw_string message

(** [setup_messages ?fs ?x messages] is a multiline message with font_size [fs] 
    starting at the x coordinate [x]. *)
let setup_messages ?fs:(fs = 18) ?x:(x = 150) messages = 
  font_size fs;
  set_color black;
  ignore (List.fold_left (fun acc m ->
      moveto x (500 - acc * 25);
      draw_string m;
      acc + 1
    ) 0 messages); ()

(** [setup_input in] creates a pseudo-input field with the current input being 
    [in]. *)
let setup_input input = 
  font_size 24;
  set_color black;
  fill_rect 300 265 200 60;
  set_color white;
  moveto (300 + 12) (255 + 15); 
  draw_string input

(** [show_error error] displays the error message corresponding to [error] on 
    the GUI. *)
let show_error error =  
  font_size 20;
  set_color red;
  let message = match error with
    | KoException -> "Ko Exception: "
    | SelfCaptureException -> "Self Capture Exception: "
    | StoneAlreadyExistsException -> "Stone Already Exists Exception: "
    | TimeExpiredException -> "Time Expired Exception: "
    | _ -> "Unknown Exception: "
  in 
  moveto (2 * !b_dims.spacing) 15;
  draw_string (message ^ "This is an illegal move.")

(** [show_winner g] displays the winner of game [g]. *)
let show_winner game =
  let color, winner = match score game with
    | x, y when x > y -> black, fst (names game) ^ " has won!"
    | x, y when x < y -> white, snd (names game) ^ " has won!"
    | x, y -> black, "The game is a draw"
  in
  font_size 48; 
  set_color color;
  moveto 150 150;
  draw_string winner

(** [setup_dims game] determines the optimal window dimensions and line spacing 
    for the graphical user interface given game [game]. 
    Requires: this function must be called before all other game or window 
      specific functions. *)
let setup_dims game = 
  let board_size = bounds game in
  let spacing = 
    (float_of_int window_size) /. (3.0 +. float_of_int board_size) 
    |> Float.ceil 
    |> int_of_float in
  let length = spacing * (board_size + 3) in
  let radius = spacing / 2 - 2 in
  b_dims.contents <- {
    board_size = board_size;
    length = length;
    radius = radius;
    spacing = spacing;
  }

(** [setup_grid] is the basic grid layout with dimensions determined previously 
    in [setup_dims]. *)
let setup_grid () = 
  let lines = Array.make !b_dims.board_size (0, 0, 0, 0) in
  let s = !b_dims.spacing in
  for i = 0 to Array.length lines - 1 do
    lines.(i) <- 
      ((2 + i) * s, 2 * s, 
       (2 + i) * s, !b_dims.length - 2 * s)
  done;
  let segments = Array.append lines (quartet_swap lines) in 
  draw_segments segments

(** [setup_axis] is the axis labels for the game as specified in 
    [setup_dims]. *)
let setup_axis () = 
  font_size 18;
  let label_axis axis = 
    for j = 0 to 1 do
      let offset = if j = 0 then 0 else !b_dims.length in
      for i = 0 to !b_dims.board_size - 1 do
        let loc = 
          ((2 + i) * !b_dims.spacing), (offset - !b_dims.spacing |> Int.abs) in
        let s = match axis, loc with
          | X, (x, y) ->
            let y_offset = if j = 1 then 16 else 5 in
            moveto (x - 5) (y - y_offset); 
            Char.escaped (Char.chr (65 + i))
          | Y, (x, y) -> 
            let x_offset = if j = 1 then 10 else 5 in
            moveto (y - x_offset) (x - 9); 
            string_of_int (!b_dims.board_size - i)
        in draw_string s;
      done
    done;
  in label_axis X; label_axis Y

(** [setup_stars g] places the reference points, sometimes called stars, on the 
    Go board as specified in [setup_dims]. *)
let setup_stars game = 
  let n1, n2, n3 = star_locations game in
  let stars = 
    if !b_dims.board_size <= 7 then cartesian [n1; n2] [n1; n2]
    else if !b_dims.board_size <= 13 then 
      (n3, n3) :: cartesian [n1; n2] [n1; n2]
    else cartesian [n1; n2; n3] [n1; n2; n3]
  in
  let coordinates = List.map (fun (c, r) -> coordinate c r) stars 
  in List.iter (fun (x, y) -> draw_stone x y 3 black) coordinates

(** [setup_handicap g h] places [h] stones on a Go board at the beginning of a 
    Go game. 
    Requires: 2 <= [h] <= 9. *)
let setup_handicap game h =
  let h_positions = handicap_c game h in
  List.iter 
    (fun (c,r) -> 
       draw_stone_cr (c,r) (!b_dims.spacing / 2 - 2) black
    ) h_positions;
  if h_positions != [] then handicap game h_positions else game

(** [setup_stones g h] draws the stones already on the board in [game]. If the 
    board is empty and [h != 0], then [h] stones are placed as handicap for 
    player 1. *)
let rec setup_stones game handicap = 
  let rad = (!b_dims.spacing / 2 - 2) in
  if stones game Black = [] then 
    if handicap > 0 then setup_stones (setup_handicap game handicap) 0 
    else game
  else
    (List.iter (fun (c,r) -> draw_stone_cr (c,r) rad white) (stones game White);
     List.iter (fun (c,r) -> draw_stone_cr (c,r) rad black) (stones game Black);
     prev_ring game `Draw; game)

(** [set_score g] marks each intersection according to whether it is black, 
    white, or neutral territory on the Go board. *)
let set_score game = 
  let grid = fill_grid game in
  for r = 0 to Array.length grid - 1 do
    for c = 0 to Array.length grid.(r) - 1 do 
      match grid.(r).(c) with
      | BlackT -> draw_stone_cr (c, r) 4 black
      | WhiteT -> draw_stone_cr (c, r) 4 white
      | Neutral -> draw_square_cr (c, r) (!b_dims.spacing / 3) 
      | Empty | BlackS | WhiteS -> ()
    done
  done

(** [set_default] is the default screen display when the board is not being 
    show. *)
let set_default () = 
  clear_graph ();
  setup_background background_color;
  setup_title ()

(** [set_start] draws the main loading page for GOcaml. *)
let set_main () = 
  set_default ();
  setup_main_buttons "PLAY" "INFO"

(** [set_gocaml_info] is the window with information about GOcaml and how to 
    play. *)
let set_gocaml_info () = 
  set_default ();
  setup_messages info_message

(** [set_load_game] is the screen prompting the user whether to play a new 
    game or one loaded from a pre-existing file *)
let set_load_game () = 
  set_default ();
  setup_message "Would you like to play a new or old GOcaml game?";
  setup_main_buttons "NEW" "OLD"

(** [set_board_size] is the screen prompting the user to select the board size 
    for a new game. *)
let set_board_size () =
  set_default ();
  setup_message "Please select from the default GOcaml board sizes";
  setup_size_buttons ()

(** [set_handicap] is the input screen allowing the user to enter the handicap, 
    if any. *)
let set_handicap () =
  set_default ();
  setup_message "Please enter the black stone's handicap, if any";
  setup_input ""; ()

(** [set_file_load] is the screen prompting a user to load a file from 
    [./games/] as a pre-existing game. *)
let set_file_load () = 
  set_default ();
  setup_message "Please enter the GOcaml file name you want to load";
  setup_input ""; ()

(** [set_game g] initializes the initial board state given game [g]. *)
let set_game game = 
  setup_dims game;
  clear_graph ();
  setup_background background_color;
  setup_grid ();
  setup_axis (); 
  setup_stars game;
  setup_stones game

(** [set_info game] displays all the revelent information for game [game]. *)
let set_info game = 
  set_default ();
  let m1, m2, m3 = game_message game in
  setup_messages m1 ~fs:24;
  setup_messages m2 ~fs:24 ~x:350;
  setup_messages m3 ~fs:24 ~x:550

(** [default_game] is the default game provided as a default argument for 
    [user_input]. *)
let default_game = Yojson.Basic.from_file "games/19.json" |> from_json

(** [user_input game t] listens for user input and updates the game 
    accordingly. *)
let rec user_input ?game:(game = default_game) ?time:(time = time ()) 
    display  =
  let event = wait_next_event [Key_pressed; Button_down] in
  if event.keypressed || event.button then
    if event.key = 'q' then 
      if display = Main then exit 0 
      else (set_main (); user_input Main)
    else match display with
      | Main -> eval_main event
      | GOcamlInfo -> eval_gocaml_info event
      | LoadGame -> eval_load_game event
      | BoardSize -> eval_board_size event
      | Handicap -> eval_handicap event game
      | FileLoad -> eval_file_load event
      | Board -> eval_board event game time
      | ScoredBoard -> eval_scored_board event game time
      | Info -> eval_info event game time
      | GameOver -> user_input GameOver

(** [eval_main event] updates the window based on event [event] when on the 
    display [Main]. *)
and eval_main event =
  let k, b = event.key, main_button_press event in
  match k, b with
  | 'p', _ | _, `Left -> set_load_game (); user_input LoadGame
  | 'i', _ | _, `Right -> set_gocaml_info (); user_input GOcamlInfo
  | _, _ -> user_input Main

(** [eval_gocaml_info event] updates the window based on event [event] when on 
    the display [GOcamlInfo]. *)
and eval_gocaml_info event = 
  match event.key with
  | 'b' -> set_main (); user_input Main
  | _ -> user_input GOcamlInfo

(** [eval_load_game event] updates the window based on event [event] when on the
    display [LoadGame]. *)
and eval_load_game event = 
  let k, b = event.key, main_button_press event in
  match k, b with
  | 'b', _ -> set_main (); user_input Main
  | 'n', _ | _, `Left -> set_board_size (); user_input BoardSize
  | 'o', _ | _, `Right -> set_file_load (); user_input FileLoad
  | _, _ -> user_input LoadGame

(** [eval_board_size event] updates the window based on event [event] when on 
    the display [BoardSize]. *)
and eval_board_size event = 
  if event.keypressed then 
    match event.key with
    | 'b' -> set_load_game (); user_input LoadGame
    | _ -> user_input BoardSize
  else
    let size = size_button_press event in
    if size = None then user_input BoardSize
    else 
      let file = Printf.sprintf "./games/%d.json" (Option.get size) in
      let game = Yojson.Basic.from_file file |> from_json in
      set_handicap (); user_input Handicap ~game:(game)

(** [accept_input ?game event display] allows the user to input text on the 
    display [display] with the optional [game] already loaded. Pressing [RETURN]
    processes the inputted text. *)
and accept_input ?game:(game = default_game) event display =
  let event = ref event in
  let break = ref false in
  let str = ref "" in
  while not !break do
    (match !event.key with
     | '\r' -> break := true
     | '\b' -> str := String.sub !str 0 (max 0 ((String.length !str) - 1))
     | k -> str := !str ^ Char.escaped k);
    setup_input !str;
    if not !break then 
      event := wait_next_event [Key_pressed; Button_down] 
  done;
  match display with 
  | Handicap -> begin 
      try 
        let h = if !str = "" then 0 else int_of_string !str in
        let game' = set_game game h in
        user_input Board ~game:game'
      with
      | _ -> setup_input ""; user_input Handicap ~game:game
    end
  | FileLoad -> begin
      try 
        let game = Yojson.Basic.from_file ("./games/" ^ !str) |> from_json 
        in user_input Board ~game:(set_game game 0)
      with
      | _ -> set_file_load (); user_input FileLoad
    end
  | _ -> failwith "precondition violated"

(** [eval_handicap e g] allows the user to enter the handicap amount for the 
    new game [g]. *)
and eval_handicap event game =
  accept_input ~game:game event Handicap

(** [eval_file_load e] allows the user to enter the file to load for a 
    pre-existing game. *)
and eval_file_load event =
  accept_input event FileLoad

(** [eval_board e g t] handles any key presses when the display is currently on 
    [Board] and the player has taken [t0] so far on their move. *)
and eval_board event game t0 =
  if event.keypressed then
    match event.key with 
    | 'i' -> set_info game; user_input Info ~game:game ~time:t0
    | 'p' -> begin
        try
          let game' = step game None t0 in
          user_input Board ~game:game'
        with 
        | GameEndException -> 
          set_info game; show_winner game; user_input GameOver
      end
    | 's' -> set_score game; user_input ScoredBoard ~game:game ~time:t0
    | 'u' -> user_input Board ~game:(set_game (undo game) 0) ~time:t0
    | _ -> user_input Board ~game:game ~time:t0  
  else 
    eval_board_click event game t0

(** [eval_board_click e g t] handles any mouse clicks when the board is on 
    [Board] and updates the game [g] accordingly. *)
and eval_board_click event game t0 = 
  let column = index event.mouse_x X in
  let row = index event.mouse_y Y in
  let actual_x, actual_y = coordinate column row in
  let t1 = time () in
  try
    let game' = step game (Some (column, row)) (t1 - t0) in
    let (c1,c2) = match turn game with
      | White -> white, black
      | Black -> black, white
    in
    draw_stone actual_x actual_y !b_dims.radius c1;
    draw_ring actual_x actual_y c1 c2;
    prev_ring game `Remove;
    ignore (set_game game' 0);
    user_input Board ~game:game' ~time:t1
  with 
  | err -> 
    ignore (set_game game 0); show_error err; 
    user_input Board ~game:game ~time:t0 

(** [eval_scored_board e g t] handles user input when the board is currently 
    scored. *)
and eval_scored_board event game t0 =
  if event.keypressed then
    match event.key with
    | 'i' -> set_info game; user_input Info ~game:game ~time:t0
    | 's' -> ignore (set_game game 0); user_input Board ~game:game ~time:t0
    | _ -> user_input ScoredBoard ~game:game ~time:t0
  else user_input ScoredBoard ~game:game ~time:t0

(** [eval_info e g t] handles user input when the display is [Info]. *)
and eval_info event game t0 = 
  if event.keypressed then
    match event.key with 
    | 'i' -> user_input Board ~game:(set_game game 0) ~time:t0
    | 's' -> 
      let rand_file = (Printf.sprintf "./games/%s.json" (random_string 10)) in
      to_json game rand_file; exit 0
    | _ -> user_input Info ~game:game ~time:t0  

let main () =
  open_graph (Printf.sprintf " %dx%d" window_size window_size);
  set_window_title "GOcaml";
  set_main ();
  user_input Main

let () = main ()
