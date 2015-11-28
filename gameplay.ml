open Gamestate
open Ai
open Str

(* -----------------------------------------------------------------------------
 * Placing Ships Phase of the Game
----------------------------------------------------------------------------- *)

let place_ship (side : side) (ship : ship)
                  (c : coord) (d : dir) : side =
  failwith "must implement"

let ship_string = function
 | Jetski -> "Jetski"
 | Patrol -> "Patrol"
 | Cruiser -> "Cruiser"
 | Submarine -> "Submarine"
 | Battleship -> "Battleship"
 | Carrier -> "Carrier"

let rec place_ships (side : side) (ships : ship list) : side =
  match ships with
  | [] -> gs
  | ship::t ->
      print_newline ();
      print_endline "Placing your %s." (ship_string ship);
      print_string "Enter a coordinate for the head of your ship:  ";
      let c = String.trim (read_line ()) in
      print_string "Enter a direction for the tail of your ship to point:  ";
      let d = String.trim (read_line ()) in
      if invalid_placement c d then place_ships side ships
      else
        let new_side = place_ship side c d in
        place_ships new_side t

(* -----------------------------------------------------------------------------
 * MAIN FUNCTION
----------------------------------------------------------------------------- *)

(* Makes an initial gamestate with two sides. Both consist of a grid
 * composed on just water of size grid_size and an empty fleet. *)
let initialize_gamestate (grid_size : int) : gamestate =
  let rec init_row grid_size =
    if grid_size = 0 then []
    else (Water, Empty)::(init_row (grid_size - 1)) in
  let row = init_row grid_size in
  let rec init_grid grid_size =
    if grid_size = 0 then []
    else row::(init_row (grid_size - 1)) in
  let board = init_grid grid_size in
  in ({grid = board; ships = []}, {grid = board; ships = 0})

let main () =
  (* SHOULD ASK FOR PLAYER USERNAMES *)

  (init_side1, init_side2) = initialize_gamestate grid_size

  ships = [Jetski; Patrol; Cruiser; Submarine; Battleship; Carrier]

  (* Placing ships phase *)
  (* side1 places ships *)
  side1 = place_ships init_side1 ships
  (* side2 places ships *)
  side2 = place_ships init_side2 ships

  gamestate = (side1, side2)

  (* CALL REPL TO BEGIN ATTACK PHASE *)

let _ = main()

