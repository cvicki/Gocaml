open Command
open Game
open OUnit2

(** Test plan:
    Overall, the majority of our testing is done through manually playing the 
    game, via terminal and GUI. To do this we created several game files. While 
    the empty boards are for both testing and general play, the others were
    created to test certain features (defined below). We manually tested 
    all the commands from the Command module and saw how they interacted with 
    the Main module. However the actual parsing was tested through OUnit. 
    [step] and [undo] of the Game module were also tested manually through 
    gameplay. Many of our tests were done manually since all of the exceptions 
    in the Game would need a series of stones to be placed to be raised. This is
    difficult to write as a single line of code for OUnit to test.

    In OUnit, we tested many functions in the Game module. This was done
    through both glass box and black box testing. Using Go rules, black box 
    tests were written for [liberties] which could be determined from looking at
    an image of a board. Then after implementation, further glass box tests were
    added. This was done similarly for [score]. Other methods such as 
    [in_bounds] were just testing through black box through TTD or afterwards. 
    [from_json] and [to_json] were tested indirectly through using their 
    outputs. As mentioned above, [undo] is hard to easily test, but we still 
    achieve 83% coverage in [game.ml] despite not testing [undo] heavily.

    The nature of graphical user interfaces prevents an easy implementation of 
    automated testing. Thus, all testing we done by launching the GUI annd 
    testing a variety of inputs on each screen (i.e. key presses and mouse 
    clicks). 

    All testing of [Util] is done through the below tests. While the coverage in
    [make bisect] indicates a low coverage percentage, many of the functions are
    only used in [gui.ml] (but these are still being tested manually).

    Through our robust testing suite, and running through a large number of 
    games on the GUI and terminal, especially after implementing a new feature,
    we believe our testing approach ensures the correctness of our system. 
*)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [cmp_values] is an oUnit Test to determine whether [v1] equals [v2]. *)
let cmp_values name v1 v2 = 
  name >:: (fun _ -> assert_equal v1 v2)

(** [cmp_values] is an oUnit Test to determine whether [v1] equals [v2]. *)
let cmp_flt name v1 v2 = 
  name >:: (fun _ -> assert_equal v1 v2 ~printer: string_of_float)

(** [test_raises2 n f i1 e] is on OUnit Test to determine whether 
    [func i1] raises [error].  *)
let test_raises name func input1 error = 
  name >:: (fun _ -> assert_raises error (fun () -> func input1))

(** [test_raises2 n f i1 i2 e] is on OUnit Test to determine whether 
    [func i1 i2] raises [error].  *)
let test_raises2 name func input1 input2 error = 
  name >:: (fun _ -> assert_raises error (fun () -> func input1 input2))

let error_free name expr = 
  name >:: (fun _ -> expr)

(** [load_file file] is the Go game representation of the contents in [file]. *)
let load_game file =
  Yojson.Basic.from_file file |> from_json

(* All the game files *)
let empty_5 = load_game "games/5.json"
let empty_7 = load_game "games/7.json"
let empty_11 = load_game "games/11.json"
let empty_19 = load_game "games/19.json"
(* General mid-way game *)
let game_one = load_game "games/game_one.json"
(* Used to test liberties  *)
let corner = load_game "games/corner.json"
(* Used to test scoring and capturing stones *)
let territories = load_game "games/territories.json"
let error_test = load_game "games/error_test.json"
(* Used to test Ko violations *)
let ko_game = load_game "games/ko_game.json"
let prisoner_game = load_game "games/prisoner_test.json"
(* Used to test GameEndException*)
let game_end = step game_one None 0

let command_tests = [
  (* Converting string location to integer tuple *)
  cmp_values "A1 is (0, 0)" (0, 0) (istone_pos "A1");
  cmp_values "C3 is (2, 2)" (2, 2) (istone_pos "C3");
  cmp_values "A13 is (0, 12)" (0, 12) (istone_pos "A13");

  (* Normal Parse Tests *)
  cmp_values "play A1 is Play A1" (parse empty_19 "play A1") (Play "A1");
  cmp_values "' quit ' is Quit" (parse empty_19 " quit ") Quit;
  cmp_values "forfeit is Forfeit" (parse empty_19 "forfeit") Forfeit;
  cmp_values "pass is Pass" (parse empty_19 "pass") Pass;
  cmp_values "save file.json is Save 'file.json'" 
    (parse empty_19 "save file.json") (Save "file.json");
  cmp_values "score is Score" (parse empty_19 "score") Score;
  cmp_values "score is Score" (parse empty_19 "print") Print;

  (* Parse Exception Tests *)
  test_raises2 "Deformed Exception" parse empty_19 "pLaY A2" Deformed;
  test_raises2 "Empty Exception" parse empty_19 " " Empty;
  test_raises2 "StoneAlreadyExists Exception" parse corner "play A1" 
    StoneAlreadyExists;
  test_raises2 "GoOutOfBounds Exception" parse empty_5 "play Z1" 
    GoOutOfBounds;
  test_raises2 "GoOutOfBounds Exception" parse empty_5 "play A20" 
    GoOutOfBounds;
  test_raises "Deformed 2" istone_pos "A" Deformed;
]

let liberty_tests = [
  cmp_values "corner white (0,8) solo, 2 liberties" 
    2 (liberties corner (0,8));
  cmp_values "corner white (0,0) solo on corner, 1 liberty"
    1 (liberties corner (0,0));

  cmp_values "corner white (8,8), corner string1, 3 liberties"
    3 (liberties corner (8,8));
  cmp_values "corner white (7,8), corner string1, 3 liberties" 
    3 (liberties corner (7,8));
  cmp_values "corner white (8,7), corner string1, 3 liberties" 
    3 (liberties corner (8,7));

  cmp_values "board1 black (3,1) solo, 4 liberties" 
    4 (liberties game_one (3,1));
  cmp_values "board1 black (2,3) solo, 3 liberties" 
    3 (liberties game_one (2,3));
  cmp_values "board1 white (3,8) solo on edge, 3 liberties" 
    3 (liberties game_one (3,8));

  cmp_values "board1 white (1,2) in string" 
    5 (liberties game_one (1,2));
  cmp_values "board1 white (2,2) in string, share liberties with (1,2)" 
    5 (liberties game_one (2,2));

  cmp_values "board1 white (5,3) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (liberties game_one (5,3));
  cmp_values "board1 white (6,2) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (liberties game_one (6,2));
  cmp_values "board1 white (6,3) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (liberties game_one (6,3));
  cmp_values "board1 white (6,4) in str(5,3)(6,2)(6,3)(6,4), 5 lib" 
    5 (liberties game_one (6,4));
]

let file_tests = [
  error_free "corner -> corner.json without errors" 
    (to_json corner "./tests/supporting/corner.json");
  cmp_values "./tests/supporting/corner.json and original corner are equal"
    (load_game "./tests/supporting/corner.json") corner
]

(* GUI GAME STATE WAS TESTED MANUALLY *)

let game_tests = [
  (* In Bounds Tests *)
  cmp_values "empty_19 0,0 in bounds" (in_bounds empty_19 (0,0)) true;
  cmp_values "empty_19 18,18 in bounds" (in_bounds empty_19 (18,18)) true;
  cmp_values "empty_19 -1,-1 not in bounds" (in_bounds empty_19 (-1,-1)) false;
  cmp_values "empty_19 19,19 not in bounds" (in_bounds empty_19 (19,19)) false;

  (* Is Empty Tests *)
  cmp_values "empty_19 F8 is empty" (is_empty empty_19 (5,7)) true;
  cmp_values "corner A1 is not empty" (is_empty corner (0,0)) false;

  (* Scoring Tests *)
  (* empty boards, differnt komi tests *)
  cmp_values "corner score" (score corner) (0.0,5.5);
  cmp_values "empty_19 score" (score empty_19) (0.0, 6.5);
  (** has territories but no prisoners, komi zero *)
  cmp_flt "territories score" (fst (15., 17.)) (fst (score territories));
  cmp_flt "territories score" (snd (15., 17.)) (snd (score territories));
  (** territories b:1, w:0, prisoners b:1, w:0, komi b:0, w:5.5 *)
  cmp_values "ko_game score" (2.0, 5.5) (score ko_game);

  (* Last stone tests *)
  cmp_values "empty board" (-1,-1) (last_stone empty_5); 
  cmp_values "place a stone at (1,1)" (1,1) 
    (last_stone (step empty_19 (Some (1,1)) 1));
  cmp_values "empty board, None move" (-1,-1) 
    (last_stone (step empty_19 None 1));

  (* Handicap tests *)
  cmp_values "star_location, empty_7" (star_locations empty_7) (2, 4, 3);
  cmp_values "star_location, empty_19" (star_locations empty_19) (3, 15, 9);

  cmp_values "0 handicap, empty_19" (handicap_c empty_19 0) [];
  cmp_values "5 handicap, empty_11" 
    (cmp_set_like_lists (handicap_c empty_11 5) 
       [(2, 2); (2, 8); (5, 5); (8, 8); (8, 2)]) true;
  cmp_values "7 handicap, empty_11" 
    (cmp_set_like_lists (handicap_c empty_11 7) 
       [(2, 2); (2, 8); (5, 5); (8, 8); (8, 2); (2, 5); (8, 5)]) true;  

  (* Invalid Input test *)
  test_raises2 "SelfCaptureException" (step error_test) (Some (0, 0)) 0 
    SelfCaptureException;
  test_raises2 "StoneAlreadyExists" (step error_test) (Some (0, 1)) 0 
    StoneAlreadyExistsException;
  test_raises2 "KoExceptoin" (step ko_game) (Some (5, 3)) 0 KoException;

  (* Two passes to end game *)
  test_raises2 "GameEndException" (step game_end) None 0 GameEndException;

  (* Undo test *)
  cmp_values "undo first play" empty_5 (undo (step empty_5 (Some (0, 0)) 0));
  cmp_values "undo ko" ko_game (undo (step ko_game (Some (0, 0)) 0));

  (* Remove prisoners *)
  cmp_flt "capture and remove prisoner" 
    (step prisoner_game (Some (7, 3)) 0 |> score |> snd) 8.5;
]

let suite =
  "GOcaml Test Suite"  >::: List.flatten [
    command_tests;
    liberty_tests;
    file_tests;
    game_tests;
  ]

let _ = run_test_tt_main suite
