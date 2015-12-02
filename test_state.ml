open Gamestate


(* =============================================================================
 * TESTING SUITE for Game State
============================================================================= *)

(*
  Unless otherwise noted, test cases operate by default on a 4x4 grid.
*)

(*
let is_error v : bool =
  match v with
  | GameCrash _ -> true
  | _ -> false
*)




(* =============================================================================
 * Reusable Variables
============================================================================= *)

let pl1 = Player1
let pl2 = Player2



(* -----------------------------------------------------------------------------
 * Reusable Variables - Row Setup
----------------------------------------------------------------------------- *)

let rw_empty = [
  (Water, Empty); (Water, Empty); (Water, Empty); (Water, Empty)
]

let rw_miss = [
  (Water, Miss); (Water, Miss); (Water, Miss); (Water, Miss)
]

let rw_miss_fst = [
  (Water, Miss); (Water, Empty); (Water, Empty); (Water, Empty)
]

let rw_empty_ship = [
  (ShipPart(Cruiser), Empty); (ShipPart(Cruiser), Empty); (ShipPart(Cruiser), Empty);
  (ShipPart(Jetski), Empty)
]

let rw_hit = [
  (ShipPart(Cruiser), Hit); (ShipPart(Cruiser), Hit); (ShipPart(Cruiser), Hit);
  (ShipPart(Jetski), Hit)
]

let rw_hit_lst = [
  (ShipPart(Cruiser), Empty); (ShipPart(Cruiser), Empty); (ShipPart(Cruiser), Empty);
  (ShipPart(Jetski), Hit)
]

let rw_mix = [
  (Water, Empty); (Water, Miss);
  (ShipPart(Patrol), Hit); (ShipPart(Patrol), Empty)
]

let rw_empty_str =    "----"
let rw_miss_str =     "oooo"
let rw_miss_fst_str = "o---"
let rw_hit_str =      "XXXX"
let rw_hit_lst_str =  "###X"
let rw_mix_str =      "-oX#"

let rw_hit_lst_str_public =  "---X"
let rw_mix_str_public =      "-oX-"


(* -----------------------------------------------------------------------------
 * Reusable Variables - Grid Setup
----------------------------------------------------------------------------- *)

let gr_empty = [rw_empty; rw_empty; rw_empty; rw_empty]
let gr_miss_fst = [rw_miss_fst; rw_empty; rw_empty; rw_empty]
let gr_miss_fst_row = [rw_miss; rw_empty; rw_empty; rw_empty]
let gr_hit_lst = [rw_empty; rw_empty; rw_empty; rw_hit_lst]
let gr_miss_row = [rw_empty; rw_miss; rw_empty; rw_hit]
let gr_hit_row = [rw_empty; rw_empty; rw_hit; rw_empty]
let gr_mix = [rw_miss_fst; rw_empty; rw_mix; rw_hit_lst]
let gr_miss_hit_empty = [rw_miss_fst; rw_hit_lst; rw_empty; rw_mix]
let gr_empty_ship = [rw_empty_ship; rw_empty; rw_empty_ship; rw_empty]

(* "----\n----\n----\n----\n" *)
let gr_empty_str =
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n"

(* "o---\n----\n----\n----\n" *)
let gr_miss_fst_str =
  rw_miss_fst_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n"

(* "oooo\n----\n----\n----\n" *)
let gr_miss_fst_row_str =
  rw_miss_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n"

(* "----\n----\n----\n###X\n" *)
let gr_hit_lst_str =
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_hit_lst_str ^ "\n"

(* "----\noooo\n----\n----\n" *)
let gr_miss_row_str =
  rw_empty_str ^ "\n" ^
  rw_miss_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n"

(* "----\n----\nXXXX\n----\n" *)
let gr_hit_row_str =
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_hit_str ^ "\n" ^
  rw_empty_str ^ "\n"

(* "o---\n----\n-oX#\n###X\n" *)
let gr_mix_str =
  rw_miss_fst_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_mix_str ^ "\n" ^
  rw_hit_lst_str ^ "\n"

(* -----------------------------------------------------------------------------
 * Reusable Variables - Side Setup
----------------------------------------------------------------------------- *)

let sd_empty = {board = gr_empty; ships = [Jetski]}
let sd_miss_fst = {board = gr_miss_fst; ships = [Jetski]}
let sd_hit_lst = {board = gr_hit_lst; ships = [Jetski]}
let sd_miss_row = {board = gr_miss_row; ships = [Jetski]}
let sd_hit_row = {board = gr_hit_row; ships = [Jetski; Cruiser]}

let sd_mix = {board = gr_mix; ships = [Jetski; Cruiser]}

let sd_hit_row_new = {board = gr_miss_hit_empty; ships = [Jetski; Cruiser]}

let sd_mix_new = {board = gr_mix; ships = [Jetski; Cruiser]}

(* =============================================================================
 * TEST - Display Gamestate
============================================================================= *)

TEST = display_gamestate (sd_empty, sd_empty) pl1 true false = gr_empty_str
TEST = display_gamestate (sd_empty, sd_empty) pl2 true false = gr_empty_str
TEST = display_gamestate (sd_empty, sd_mix) pl1 true false = gr_empty_str
TEST = display_gamestate (sd_empty, sd_mix) pl2 true false = gr_mix_str
TEST = display_gamestate (sd_miss_fst, sd_hit_lst) pl1 true false = gr_miss_fst_str
TEST = display_gamestate (sd_miss_fst, sd_hit_lst) pl2 true false = gr_hit_lst_str
TEST = display_gamestate (sd_miss_row, sd_hit_row) pl2 true false = gr_hit_row_str

TEST = display_gamestate (sd_empty, sd_hit_lst) pl1 false false
        = "----\n----\n----\n---X\n"

TEST = display_gamestate (sd_empty, sd_mix) pl1 false false
        = "o---\n----\n-oX-\n---X\n"

TEST = display_gamestate (sd_hit_lst, sd_empty) pl2 false false
        = "----\n----\n----\n---X\n"

TEST = display_gamestate (sd_mix, sd_empty) pl2 false false
        = "o---\n----\n-oX-\n---X\n"


(* =============================================================================
 * TEST - Victory Check
============================================================================= *)

TEST = victory (sd_hit_row_new, sd_mix_new) = None

TEST = victory (sd_hit_row_new, sd_hit_row_new) = None

TEST = victory (sd_hit_row_new, sd_hit_row) = Some (Player1)

TEST = victory (sd_hit_row, sd_hit_row_new) = Some (Player2)

(* =============================================================================
 * TEST - Turn
============================================================================= *)

let c = (sd_empty, sd_empty)
let (b,c) = turn c ('a',0) Player1

TEST = b = Some Miss
TEST = display_gamestate c pl2 true false = gr_miss_fst_str
TEST = display_gamestate c pl1 true false = gr_empty_str

let (b,c) = turn c ('a', 0) Player1

TEST = b = None
TEST = display_gamestate c pl2 true false = gr_miss_fst_str
TEST = display_gamestate c pl1 true false = gr_empty_str

let (b,c) = turn c ('a',1) Player1
let (b,c) = turn c ('a',2) Player1
let (b,c) = turn c ('a',3) Player1


TEST = b = Some Miss
TEST = display_gamestate c pl2 true false = gr_miss_fst_row_str
TEST = display_gamestate c pl1 true false = gr_empty_str

let sd_empty_ship = {board = gr_empty_ship; ships = [Jetski; Cruiser]}

let c = (sd_empty, sd_empty_ship)
let (b,c) = turn c ('a',0) Player1

TEST = b = Some Hit

TEST = display_gamestate c pl2 true false = "X###\n----\n####\n----\n"

let (b,c) = turn c ('a',1) Player1
let (b,c) = turn c ('a',2) Player1
let (b,c) = turn c ('a',3) Player1
let (b,c) = turn c ('b',0) Player1
let (b,c) = turn c ('b',1) Player1
let (b,c) = turn c ('b',2) Player1
let (b,c) = turn c ('b',3) Player1

TEST = b = Some Miss
TEST = display_gamestate c pl2 true false = "XXXX\noooo\n####\n----\n"


(*TEST = print_bytes (display_gamestate c pl2 true false); true*)


(*gamestate line 229*)
