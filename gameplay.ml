open Gamestate
open Ai
open Str

(* -----------------------------------------------------------------------------
 * PLACING SHIPS PHASE
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

let translate_coord (s : string) : coord option =
  failwith "must implement"

let translate_dir (s : string) : dir option =
  failwith "must implement"

let rec place_ships (side : side) (ships : ship list) : side =
  match ships with
  | [] -> side
  | ship::t ->
      print_newline ();
      Printf.printf "Placing your %s.\n" (ship_string ship);
      print_string "Enter a coordinate for the head of your ship:  ";
      let c = translate_coord (String.trim (read_line ())) in
      print_string "Enter a direction for the tail of your ship to point:  ";
      let d = translate_dir (String.trim (read_line ())) in
      begin match c,d with
      | None,_ ->
          print_endline "These are invalid coordinates. Try Again";
          place_ships side ships
      | _,None ->
          print_endline "This is an invalid direction. Try Again";
          place_ships side ships
      | Some(c),Some(d) ->
          let new_side = place_ship side ship c d in
          place_ships new_side t
      end


(* -----------------------------------------------------------------------------
 * ATTACK PHASE
----------------------------------------------------------------------------- *)

let interp_input (gs : gamestate) (instr : string) : gamestate =
  failwith "TODO"

let repl (gs : gamestate) : unit =
  failwith "TODO"

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
    else row::(init_grid (grid_size - 1)) in
  let b = init_grid grid_size in
  ({board = b; ships = []}, {board = b; ships = []})

let main () =
  (* SHOULD ASK FOR PLAYER USERNAMES *)

  let (init_side1, init_side2) = initialize_gamestate grid_size in

  let ships = [Jetski; Patrol; Cruiser; Submarine; Battleship; Carrier] in

  (* Placing ships phase *)
  (* side1 places ships *)
  let side1 = place_ships init_side1 ships in
  (* side2 places ships *)
  let side2 = place_ships init_side2 ships in

  let gamestate = (side1, side2) in
  ignore(gamestate)

  (* CALL REPL TO BEGIN ATTACK PHASE *)

let _ = main()

