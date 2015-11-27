open Gamestate
open Ai

let place_ship (side : side) (ship : ship)
                  (c : coord) (d : dir) : side =
  failwith "must implement"

let rec place_ships (side : side) (ships : ship list) : side =
  match ships with
  | [] -> gs
  | ship::t -> failwith "Ask user for input and place ship"

let main () =
  (* SHOULD ASK FOR PLAYER USERNAMES *)

  (init_side1, init_side2) = initialize_gamestate ()

  ships = [Jetski; Patrol; Cruiser; Submarine; Battleship; Carrier]

  (* Placing ships phase *)
  (* side1 places ships *)
  side1 = place_ships init_side1 ships
  (* side2 places ships *)
  side2 = place_ships init_side2 ships

  gamestate = (side1, side2)

  (* CALL REPL TO BEGIN ATTACK PHASE *)

let _ = main()

