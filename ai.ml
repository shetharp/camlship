open Gamestate

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


(**)
let best_move (tsGrid: tilestate list list): coord =
  failwith "Implement me!!!"