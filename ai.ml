open Gamestate

(* Mutable record to store information to make best next move.*)
type best_move_data = {
  mutable first_hit : coord option;
  mutable next_moves : coord list;
  mutable last_move : coord option}

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
      let (c,d) = (random_coord (), random_dir ()) in
      begin match place_ship side ship c d with
      | None -> ai_place_ships side ships
      | Some(new_side) ->
          let gs_buffer = (new_side, {board = []; ships = []}) in
          print_string (display_gamestate gs_buffer Player2 true true);
          ai_place_ships new_side t
      end


(* Always gives a valid coordinate (in bounds and not already played) *)
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

let bmdata = { first_hit = None; next_moves = []; last_move = None }

let gen_next_moves (g:grid) (c:coord) : unit =
  let is_valid_move c =
    try
      let (_,ts) = get_tile g c in
      ts = Empty
    with
      | _ -> false
  in
  let uptile = (Char.chr (Char.code (fst c) - 1), snd c) in
  let downtile = (Char.chr (Char.code (fst c) + 1), snd c) in
  let lefttile = (fst c, (snd c) - 1) in
  let righttile = (fst c, (snd c) + 1) in
  let moves =
    (if (is_valid_move uptile) then [uptile] else []) @
    (if (is_valid_move downtile) then [downtile] else []) @
    (if (is_valid_move lefttile) then [lefttile] else []) @
    (if (is_valid_move righttile) then [righttile] else []) in
  let random_moves = List.sort
                    (fun x y -> if Random.bool () then 1 else -1) moves in
  bmdata.next_moves <- random_moves

(* Updates bmdata - eventually: randomize the way tiles are added in, add logic
   that removes tiles in the wrong direction *)
let update_bmdata g : unit =
  match bmdata.last_move with
  | None -> ()
  | Some c -> let (_,ts) = get_tile g c in
      begin match ts with
      | Hit ->
          begin match bmdata.first_hit with
          | None -> bmdata.first_hit <- Some c;
                    gen_next_moves g c
          | Some _ -> () (*Add logic here to remove tiles in wrong direction*)
          end
      | _ -> ()
      end

(* Returns the best tile to hit.
 *    - If it knows of a hit on the grid that is not a sunken ship then
 *      it will choose a spot adjacent to that hit
 *    - If there are no known hits then it will randomly choose a spot
 *      at least two tiles away - implement this part??? *)
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

(*Needs to take in last move*)
let make_move (g:grid) (easy:bool): coord =
  if easy then rand_move g
  else best_move g
