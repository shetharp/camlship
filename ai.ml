open Gamestate

(* Mutable record to store information to determine the best next move.*)
type best_move_data = {
  mutable first_hit : coord option;
  mutable next_moves : coord list;
  mutable last_move : coord option}

(* A random direction value *)
let random_dir () : dir =
  let i = Random.int 4 in
  if i = 0 then Up
  else if i = 1 then Down
  else if i = 2 then Right
  else if i = 3 then Left
  else failwith "random failure"

(* A random coordinate *)
let random_coord () : coord =
  (Char.chr (Random.int grid_size + 65), Random.int grid_size)

(* Are these row and col values within the grid *)
let in_grid (row, col) : bool =
  if (row < 0 || row >= grid_size) || (col < 0 || col >= grid_size)
  then false else true

let make_coord (row, col) : coord = (Char.chr (65 + row), col)

(* Checks the four adjacent tiles for ships, and returns true if that
 * is the case *)
let look_around g c : bool =
  let row = (Char.code (fst c)) - 65 in
  let col = snd c in
  let adj1 = ((row - 1), col) in
  let adj2 = ((row + 1), col) in
  let adj3 = (row, (col - 1)) in
  let adj4 = (row, (col + 1)) in
  let tile1 =
    if in_grid adj1 then fst (get_tile g (make_coord adj1)) else Water in
  let tile2 =
    if in_grid adj2 then fst (get_tile g (make_coord adj2)) else Water in
  let tile3 =
    if in_grid adj3 then fst (get_tile g (make_coord adj3)) else Water in
  let tile4 =
    if in_grid adj4 then fst (get_tile g (make_coord adj4)) else Water in
  match tile1, tile2, tile3, tile4 with
  | ShipPart(_),_,_,_ -> true
  | _,ShipPart(_),_,_ -> true
  | _,_,ShipPart(_),_ -> true
  | _,_,_,ShipPart(_) -> true
  | _,_,_,_           -> false

(* Returns true if there is a ship adjacent to one of the spots in the
 * possible ship placement *)
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
          let rand = Random.bool () in
          if (adjacent_ship (side.board) c d (ship_length ship) && rand)
          then ai_place_ships side ships
          else ai_place_ships new_side t
        end


(* Returns a random coordinate, that is valid: i.e. in bounds and not already
 * played. *)
let rand_move (g:grid): coord =
  let len = grid_size in
  let rec get_valid_coord (): coord =
    let x = Random.int len in
    let y = Random.int len in
    let (_,ts) = List.nth (List.nth g y) x in
    if ts = Empty
    then (Char.chr ((Char.code 'A') + y), x)
    else get_valid_coord ()
  in
  get_valid_coord ()

let is_valid_move (g:grid) (c:coord) =
  try
    let (_,ts) = get_tile g c in
    ts = Empty
  with
    | _ -> false

let bmdata = { first_hit = None; next_moves = []; last_move = None }

(* Sets the bmdata.next_moves field to valid tiles neighboring the given coord.
 *)
let gen_next_moves (g:grid) (c:coord) : unit =
  let uptile = (Char.chr (Char.code (fst c) - 1), snd c) in
  let downtile = (Char.chr (Char.code (fst c) + 1), snd c) in
  let lefttile = (fst c, (snd c) - 1) in
  let righttile = (fst c, (snd c) + 1) in
  let moves =
    (if (is_valid_move g uptile) then [uptile] else []) @
    (if (is_valid_move g downtile) then [downtile] else []) @
    (if (is_valid_move g lefttile) then [lefttile] else []) @
    (if (is_valid_move g righttile) then [righttile] else []) in
  let random_moves = List.sort
                    (fun x y -> if Random.bool () then 1 else -1) moves in
  bmdata.next_moves <- random_moves

(* Updates bmdata's fields based on the previous moves and whether they were
 * hits or misses. *)
let update_bmdata g : unit =
  match bmdata.last_move with
  | None -> ()
  | Some c -> let (_,ts) = get_tile g c in
      begin match ts with
      | Hit ->
          begin match bmdata.first_hit with
          | None -> bmdata.first_hit <- Some c;
                    gen_next_moves g c
          | Some i -> if (fst i = fst c) (* rows same*)
                      then bmdata.next_moves <-
                        let nextc =
                          if ((snd c) > (snd i))
                          then (fst c, (snd c) + 1)
                          else (fst c, (snd c) - 1)
                        in
                        (
                          if is_valid_move g nextc
                          then [nextc]
                          else []
                        )@
                        (List.filter (fun (x,_) -> x = (fst i)) bmdata.next_moves)
                      else (*cols same*)
                        bmdata.next_moves <-
                        let nextc =
                          if ((Char.code (fst c)) > (Char.code (fst i)))
                          then (Char.chr (Char.code(fst c) + 1), snd c)
                          else (Char.chr (Char.code(fst c) - 1), snd c)
                        in
                        (
                          if is_valid_move g nextc
                          then [nextc]
                          else []
                        )@
                        (List.filter (fun (_,y) -> y = (snd i)) bmdata.next_moves)
          end
      | _ -> ()
      end

(* Returns the best tile to hit.
 *    - If it knows of a hit on the grid that is not a sunken ship then
 *      it will choose a spot adjacent to that hit
 *    - If there are no known hits then it will randomly choose a spot*)
let best_move (g: grid): coord =
  if bmdata.last_move = None then (* First move of the game*)
    let c = rand_move g in
    bmdata.last_move <- Some c;
    c
  else
    let _ = update_bmdata g in
    match bmdata.next_moves with
    | []   -> bmdata.first_hit <- None;
              let c = rand_move g in
              bmdata.last_move <- Some c;
              c
    | h::t -> bmdata.last_move <- Some h;
              bmdata.next_moves <- t;
              h

(* If easy is true, then AI plays randomly. If easy is false, AI uses our
 * designed algorithm. *)
let make_move (g:grid) (easy:bool): coord =
  if easy then rand_move g
  else best_move g
