open Gamestate

(**)
let ai_place_ships () : grid * fleet =
  failwith "Implement me!!! "

(* Returns the best tile to hit.
 *    - If it knows of a hit on the grid that is not a sunken ship then
 *      it will choose a spot adjacent to that hit
 *    - If there are no known hits then it will randomly choose a spot
 *      at least two tiles away *)
let best_move (g: grid): coord =
  failwith ""

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


(*Needs to take in last move*)
let make_move (g:grid) (easy:bool): coord =
  if easy then rand_move g
  else best_move g