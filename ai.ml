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

let in_grid (row, col) : bool =
  if (row < 0 || row >= grid_size) || (col < 0 || col >= grid_size)
  then false else true

let make_coord (row, col) : coord = (Char.chr (65 + row), col)

let look_around g c d : bool =
  let row = (Char.code (fst c)) - 65 in
  let col = snd c in
  let adj1 = ((row - 1), col) in
  let adj2 = ((row + 1), col) in
  let adj3 = (row, (col - 1)) in
  let adj4 = (row, (col + 1)) in
  let tile1 =
    if in_grid adj1 then fst (get_tile g (make_coord adj1) d) else Water in
  let tile2 =
    if in_grid adj2 then fst (get_tile g (make_coord adj2) d) else Water in
  let tile3 =
    if in_grid adj3 then fst (get_tile g (make_coord adj3) d) else Water in
  let tile4 =
    if in_grid adj4 then fst (get_tile g (make_coord adj4) d) else Water in
  match tile1, tile2, tile3, tile4 with
  | ShipPart(_),_,_,_ -> true
  | _,ShipPart(_),_,_ -> true
  | _,_,ShipPart(_),_ -> true
  | _,_,_,ShipPart(_) -> true
  | _,_,_,_           -> false

let rec adjacent_ship (g : grid) (c : coord) (d : dir) (i : int) : bool =
  if i = 0 then false
  else
    if look_around g c then true
    else
      let row = (Char.code (fst c)) - 65 in
      let col = snd c in
      let (new_row, new_col) =
        begin match d with
        | Down -> (row + 1, col)
        | Up -> (row - 1, col)
        | Left -> (row, col - 1)
        | Right -> (row, col + 1)
        end in
      let new_c = (Char.chr (new_row + 65), new_col) in
    adjacent_ship g new_c d (i-1)

(* Algorithm for AI to place its ships. *)
let rec ai_place_ships (side : side) (ships : fleet) : side =
  match ships with
  | [] -> side
  | ship::t ->
      let (c,d) = (random_coord (), random_dir ()) in
      begin match place_ship side ship c d with
      | None -> ai_place_ships side ships
      | Some(new_side) ->
          if adjacent_ship (side.board) c d (ship_length ship)
          then ai_place_ships side ships
          else
            let gs_buffer = (new_side, {board = []; ships = []}) in
            print_string (display_gamestate gs_buffer Player2 true true);
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
