open Gamestate

(* Mutable record to store information to make best next move.*)
type best_move_data = {
  mutable first_hit_streak : coord option;
  mutable next_moves : coord list}

let random_dir () : dir =
  let i = Random.int 4 in
  if i = 0 then Up
  else if i = 1 then Down
  else if i = 2 then Right
  else if i = 3 then Left
  else failwith "random failure"

let random_coord () : coord =
  (Char.chr (Random.int grid_size + 65), Random.int grid_size)

(**)
let rec ai_place_ships (side : side) (ships : fleet) : side =
  match ships with
  | [] -> side
  | ship::t ->
      Printf.printf "Computer placing %s\n" (ship_string ship);
      let (c,d) = (random_coord (), random_dir ()) in
      begin match place_ship side ship c d with
      | None -> ai_place_ships side ships
      | Some(new_side) ->
          let gs_buffer = (new_side, {board = []; ships = []}) in
          print_endline (display_gamestate gs_buffer Player2 true true);
          ai_place_ships new_side t
      end


(* Always gives a valid coordinate (in bounds and ) *)
let rand_move (g:grid): coord =
  let len = List.length g in
  let rec get_valid_coord (): coord =
    let x = Random.int len in
    let y = Random.int len in
    let (_,ts) = List.nth (List.nth g y) x in
    if ts = Empty
    then (Char.chr ((Char.code 'A') + y), x)
    else get_valid_coord ()
  in
  get_valid_coord ()

let bmdata = { first_hit_streak = None; next_moves = [] }

(* Returns the best tile to hit.
 *    - If it knows of a hit on the grid that is not a sunken ship then
 *      it will choose a spot adjacent to that hit
 *    - If there are no known hits then it will randomly choose a spot
 *      at least two tiles away *)
let best_move (g: grid): coord =
  rand_move g

(*bmdata.first_hit_streak <- Some*)

(*Needs to take in last move*)
let make_move (g:grid) (easy:bool): coord =
  if easy then rand_move g
  else best_move g
