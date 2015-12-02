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

(* Returns an uppercase character and a number as a coord option and a direction option *)
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

let rec place_ships (side : side) (ships : ship list) : side =
  match ships with
  | [] -> side
  | ship::t ->
      print_newline ();
      Printf.printf "Placing your %s. Enter a coordinate for the head of your ship
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
          begin match place_ship side ship c d with
          | None ->
              (print_endline "These coordinates are out of bounds
or your ship overlaps with another. Try Again";
              place_ships side ships)
          | Some(new_side) ->
              let gs_buffer = (new_side, {board = []; ships = []}) in
              print_endline (display_gamestate gs_buffer Player1 true);
              place_ships new_side t
          end
      end


(* -----------------------------------------------------------------------------
 * ATTACK PHASE
----------------------------------------------------------------------------- *)

let display_boards (gs : gamestate) (p : player) : gamestate =
  let opp = display_gamestate gs p false in
  let own = display_gamestate gs p true in
  print_endline "Opponent's board:";
  print_endline opp;
  print_endline "Your board:";
  print_endline own; gs

let try_move (gs : gamestate) (s: string) (p : player) : gamestate * bool=
  let c = String.get s 0 in
  let is = String.get s 1 in
  try
    let i = int_of_char is in
    let (t, gnew) = turn gs (c,i) p in
    match t with
    | None -> print_endline "[!] Invalid move. Try again!!"; (gs, false)
    | Some v -> (
      match v with
      | Empty -> print_endline "[!] Invalid move. Try again"; (gs, false)
      | Hit   -> print_endline "You hit a ship! Your turn again!"; (gnew, false)
      | Miss  -> print_endline "You missed."; (gnew, true)
    )
  with _ -> print_endline "[!] Invalid move. Try again."; (gs, false)


(*returns updated gamestate and bool true if next player's turn*)
let interp_input (gs : gamestate) (p : player) (instr : string)
  : (gamestate * bool)=
  let open Str in
  let inp = Str.split (Str.regexp " ") instr in
  match inp with
  | [] -> print_endline "[!] Please input a command. Try again"; (gs, false)
  | hd :: [] -> (
    match hd with
    | "show"  -> ((display_boards gs p), false)
    | "board" -> ((display_boards gs p), false)
    | _       -> (try_move gs hd p)
  )
  | _ -> print_endline "[!] Invalid command. Try again"; (gs, false)

let rec repl (gs : gamestate) (ps : playerstate) (continue : bool): unit =
  let v = victory gs in
  match v with
  | Some Player1 -> print_endline ("[!!] Congratulations! "^ps.first^" has won!")
  | Some Player2 -> print_endline ("[!!] Congratulations! "^ps.second^" has won!")
  | None -> begin
    if (not continue)
    then (print_endline "The game has ended. Thanks for playing!";)
    else begin
      match ps.current with
      | Player1 -> (print_endline (ps.first^"'s turn. Make your move.");
        let _ = display_boards gs ps.current in
        let read = read_line () in
        let trimmed = String.trim read in
        let input = String.lowercase trimmed in
        if input = "quit" then repl gs ps false
        else let (newgs, switchPlayer) = interp_input gs ps.current input in
        let newcurp = (if switchPlayer
        then if ps.current = Player1 then Player2 else Player1
        else ps.current) in
        let newps = {first = ps.first; second = ps.second; current = newcurp} in
        repl newgs newps true
        )
      | Player2 -> (print_endline (ps.second^"'s turn. Make your move.");
        repl gs ps true)
      end
  end
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

let generate_fleet () : fleet =
    if grid_size >= 10 then [Carrier; Battleship; Submarine; Cruiser; Patrol; Jetski]
    else if grid_size >= 9 then [Battleship; Submarine; Cruiser; Patrol; Jetski]
    else if grid_size >= 8 then [Submarine; Cruiser; Patrol; Jetski]
    else if grid_size >= 6 then [Cruiser; Patrol; Jetski]
    else if grid_size >= 4 then [Patrol; Jetski]
    else if grid_size >= 2 then [Jetski]
    else []

let main () =
  (*TYPE IN DIRECTIONS*)

  print_endline "Please enter player name.";

  let name = read_line () in

  let ps = {first = name; second = "Computer"; current = Player1} in

  let (init_side1, init_side2) = initialize_gamestate () in

  let ships = generate_fleet () in

  (* Placing ships phase *)
  (* side1 places ships *)
  Printf.printf "%s, place your ships!" ps.first;
  let side1 = place_ships init_side1 ships in
  (* side2 places ships *)
  Printf.printf "%s, place your ships!" ps.second;
  let side2 = place_ships init_side2 ships in

  let gamestate = (side1, side2) in

  repl gamestate ps true

let _ = main()

