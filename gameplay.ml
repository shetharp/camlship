open Gamestate
open Ai
open Str

(* -----------------------------------------------------------------------------
 * PLACING SHIPS PHASE
----------------------------------------------------------------------------- *)
let ship_string = function
 | Jetski -> "Jetski"
 | Patrol -> "Patrol"
 | Cruiser -> "Cruiser"
 | Submarine -> "Submarine"
 | Battleship -> "Battleship"
 | Carrier -> "Carrier"

let dir_of_string (s : string) : dir option =
  if s = "DOWN" then Some(Down)
  else if s = "UP" then Some(Up)
  else if s = "LEFT" then Some(Left)
  else if s = "RIGHT" then Some(Right)
  else None

let translate (instr : string) : coord option * dir option =
  let words = List.map (String.trim)
                (Str.bounded_split (Str.regexp " ") instr 2) in
  match words with
  | [] -> (None, None)
  | h::[] -> (None, None)
  | c::d::t ->
      let c_option =
        if String.length c < 2 then None
        else
          let letter = String.get (String.uppercase c) 0 in
          if (Char.code letter) < 65 || (Char.code letter) > 90
          then None
          else
            let num_string = String.sub c 1 ((String.length c) - 1) in
            let num =
              try int_of_string num_string with
              | exn -> -1 in
            if num < 0 then None
            else Some(letter, num) in
      let d_option = dir_of_string (String.uppercase d) in
      (c_option, d_option)

let out_of_bounds ((letter,number) : coord) (d : dir) : bool =
  if number > grid_size || number < 1 then false
  else if ((Char.code letter) - 64) > grid_size then false
  else true

let rec place_ships (side : side) (ships : ship list) : side =
  match ships with
  | [] -> side
  | ship::t ->
      print_newline ();
      Printf.printf "Placing your %s. Enter a coordinate for the head of your ship\n
        and a direction for the tail of your ship to point:  " (ship_string ship);
      let (c,d) = translate (String.trim (read_line ())) in
      begin match c,d with
      | None,None ->
          print_endline "These are invalid instructions. Try Again";
          place_ships side ships
      | None,_ ->
          print_endline "These are invalid coordinates. Try Again";
          place_ships side ships
      | _,None ->
          print_endline "This is an invalid direction. Try Again";
          place_ships side ships
      | Some(c),Some(d) ->
          if out_of_bounds c d then
            (print_endline "These coordinates are out of bounds. Try Again";
            place_ships side ships)
          else
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
let initialize_gamestate () : gamestate =
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

  let (init_side1, init_side2) = initialize_gamestate () in

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

