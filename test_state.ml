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

let pl1 = Player1("John")
let pl2 = Player2("Jane")



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

let rw_hit_lst_str_alt =  "---X"
let rw_mix_str_alt =      "-oX-"


(* -----------------------------------------------------------------------------
 * Reusable Variables - Grid Setup
----------------------------------------------------------------------------- *)

let gr_empty = [rw_empty; rw_empty; rw_empty; rw_empty]
let gr_miss_fst = [rw_miss_fst; rw_empty; rw_empty; rw_empty]
let gr_hit_lst = [rw_empty; rw_empty; rw_empty; rw_hit_lst]
let gr_miss_row = [rw_empty; rw_miss; rw_empty; rw_empty]
let gr_hit_row = [rw_empty; rw_empty; rw_hit; rw_empty]
let gr_mix = [rw_miss_fst; rw_empty; rw_mix; rw_hit_lst]

let gr_empty_str =
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n"

let gr_miss_fst_str =
  rw_miss_fst_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n"

let gr_hit_lst_str =
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_hit_lst_str ^ "\n"

let gr_miss_row_str =
  rw_empty_str ^ "\n" ^
  rw_miss_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n"

let gr_hit_row_str =
  rw_empty_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_hit_str ^ "\n" ^
  rw_empty_str ^ "\n"

let gr_mix_str =
  rw_miss_fst_str ^ "\n" ^
  rw_empty_str ^ "\n" ^
  rw_mix_str ^ "\n" ^
  rw_hit_lst_str ^ "\n"



(* -----------------------------------------------------------------------------
 * Reusable Variables - Side Setup
----------------------------------------------------------------------------- *)

let sd_empty = {board = gr_empty; ships = [(Jetski, [('D', 4)])]}
let sd_miss_fst = {board = gr_miss_fst; ships = [(Jetski, [('D', 4)])]}
let sd_hit_lst = {board = gr_hit_lst; ships = [(Jetski, [('D', 4)])]}
let sd_miss_row = {board = gr_miss_row; ships = [(Jetski, [('D', 4)])]}
let sd_hit_row = {board = gr_hit_row; ships = [
    (Cruiser, [('C', 1)]);
    (Cruiser, [('C', 2)]);
    (Cruiser, [('C', 3)]);
    (Jetski, [('C', 4)]);
  ]}
let sd_mix = {board = gr_mix; ships = [
    (Cruiser, [('C', 1)]);
    (Cruiser, [('C', 2)]);
    (Cruiser, [('C', 3)]);
    (Jetski, [('C', 4)]);
    (Jetski, [('D', 4)])
  ]}




(* =============================================================================
 * TEST - Display Gamestate
============================================================================= *)

TEST = display_gamestate (sd_empty, sd_empty) pl1 = gr_empty_str
TEST = display_gamestate (sd_empty, sd_empty) pl2 = gr_empty_str
TEST = display_gamestate (sd_empty, sd_mix) pl1 = gr_empty_str
TEST = display_gamestate (sd_empty, sd_mix) pl2 = gr_mix_str
TEST = display_gamestate (sd_miss_fst, sd_hit_lst) pl1 = gr_miss_fst_str
TEST = display_gamestate (sd_miss_fst, sd_hit_lst) pl2 = gr_hit_lst_str
TEST = display_gamestate (sd_miss_row, sd_hit_row) pl1 = gr_miss_row_str
TEST = display_gamestate (sd_miss_row, sd_hit_row) pl2 = gr_hit_row_str
