(* Battleship game model
*)




(* =============================================================================
 * Constants (As defined in "gamestate.mli")
============================================================================= *)

(*TODO: All caps version of 'grid_size' is unacceptable for compiler.
Fix it in the gamestate.mli. Potentially unnecessary--get rid of it entirely? *)
let grid_size = 4




(* =============================================================================
 * Types (As defined in "gamestate.mli")
============================================================================= *)

type ship =
  | Jetski
  | Patrol
  | Cruiser
  | Submarine
  | Battleship
  | Carrier

type terrain = Water | ShipPart of ship

type tilestate = Hit | Miss | Empty

type grid = (terrain * tilestate) list list

type dir = Up | Down | Left | Right

type coord = char * int

type fleet = ship list

type player = Player1 | Player2

type playerstate = {first : string; second : string; current : player}

type side = {board : grid; ships : fleet}

type gamestate = side * side




(* =============================================================================
 * Game State Functions
============================================================================= *)


(* -----------------------------------------------------------------------------
 * Game State Functions - Ship Length
----------------------------------------------------------------------------- *)

(** Returns the length of a ship as an integer *)
let ship_length = function
 | Jetski -> 1
 | Patrol -> 2
 | Cruiser -> 3
 | Submarine -> 4
 | Battleship -> 5
 | Carrier -> 6


(* -----------------------------------------------------------------------------
 * Game State Functions - Turn
----------------------------------------------------------------------------- *)
(* We'll probably want a function that..
 *   (1) Checks if the coordinate is out of the grid's range
 *   (2) Translates coordinates to indices
 *)

let get_row c = (Char.code (Char.uppercase c)) - (Char.code 'A')

(* Returns an updated grid of g with the indicated coordinate replaced with ts.
   Precondition: crd is a valid coordinate *)
let replace_element (g:grid) (tile:terrain*tilestate) (crd:coord) : grid =
  let rowNum = get_row (fst crd) in
  let colNum = snd crd in
  let rec replace_in_row row acc: (terrain * tilestate) list =
    match row with
    | [] -> []
    | h::t -> if acc = colNum
              then tile :: (replace_in_row t (acc+1))
              else h :: (replace_in_row t (acc+1))
  in
  let rec replace g acc : grid =
    match g with
    | [] -> []
    | h::t -> if acc = rowNum
              then (replace_in_row h 0) :: (replace t (acc+1))
              else h :: (replace t (acc+1))
  in
  replace g 0

(*Interp will take care of checking out of bounds. - can remove that from below*)
(*Currently can handle lowercase or uppercase coords*)

(** Returns: a pair of the tilestate and new gamestate after a move has been
 *    made. If the coordinate is not a valid tile or it has already been picked,
 *    then tilestate is None and the gamestate is returned unchanged.
 *  Precondition: Player is the player that is making the move.
 *)
let turn gstate crd plyr : (tilestate option * gamestate) =
  let makeMove s c =
    let g = s.board in
    try
      let row = List.nth g (get_row (fst c)) in
      let tile = List.nth row (snd c) in
      begin match tile with
      | (Water, Empty) -> let newg = replace_element g (Water,Miss) crd in
                          let news = {board = newg; ships = s.ships} in
                          (Some Miss, news)
      | (ShipPart x, Empty) -> let newg = replace_element g (ShipPart x,Hit) crd in
                               let news = {board = newg; ships = s.ships} in
                               (Some Hit, news)
      | _ -> raise (Failure "Already hit")
      end
    with
      _ -> (None, s)
  in
  match plyr, gstate with
  | Player1, (s1,s2) -> let (a,b) = makeMove s2 crd in (a, (s1,b))
  | Player2, (s1,s2) -> let (a,b) = makeMove s1 crd in (a, (b,s2))


(* -----------------------------------------------------------------------------
 * Game State Functions - Place Ship
----------------------------------------------------------------------------- *)

let out_of_bounds ((letter,number) : coord) (d : dir) (s : ship) : bool =
  let hrow = (Char.code letter) - 65 in
  let hcol = number in
  let trow =
    match d with
    | Up -> hrow - (ship_length s) + 1
    | Down -> hrow + (ship_length s) - 1
    | _ -> hrow in
  let tcol =
    match d with
    | Left -> hcol - (ship_length s) + 1
    | Right -> hcol + (ship_length s) - 1
    | _ -> hcol in
  if (hrow > grid_size || hrow < 0) ||
     (hcol > grid_size || hcol < 0) ||
     (trow > grid_size || trow < 0) ||
     (tcol > grid_size || tcol < 0)
     then true
  else false

let overlap (g : grid) (crd : coord) : bool =
  let rowNum = (Char.code (fst crd)) - 65 in
  let colNum = snd crd in
  let rec find_in_row row acc: bool =
    match row with
    | [] -> failwith "out of range"
    | h::t -> if acc = colNum
              then begin match (fst h) with
                | ShipPart _ -> true
                | Water -> false
                end
              else find_in_row t (acc+1)
  in
  let rec find g acc : bool =
    match g with
    | [] -> failwith "out of range"
    | h::t -> if acc = rowNum
              then find_in_row h 0
              else find t (acc+1)
  in
  find g 0

let rec replace_tiles (g : grid) (ship : ship) (c : coord)
                        (d : dir) (i : int) : grid option =
  if i = 0 then Some(g)
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
    let new_coord = (Char.chr (new_row + 65), new_col) in
    if overlap g c then None
    else
      let new_grid = replace_element g (ShipPart(ship), Empty) c in
      replace_tiles new_grid ship new_coord d (i-1)

(** Returns: Returns a side option with either the updated side
      or None if the coordinates were out of bounds or overlapping
      with another ship.
    Pre: Takes in a coordinate and direction.
 * TODO: Implementation spec/comments
*)
let place_ship (sid : side) (ship : ship)
                  (c : coord) (d : dir) : side option =
  if out_of_bounds c d ship then None
  else
    let g = sid.board in
    begin match replace_tiles g ship c d (ship_length ship) with
    | Some(g) -> Some {board = g; ships = ship::(sid.ships)}
    | None -> None
    end

(* -----------------------------------------------------------------------------
 * Game State Functions - Display Gamestate
----------------------------------------------------------------------------- *)

(** Returns a string representation of the grid given a gamestate and player.
 * Legend:
 * - = Water, Empty
 * o = Water, Miss
 * X = Ship, Hit
 * # = Ship, Empty
 * ? = Error
 *
 * TODO: In the future, extend functionality to show the player their own
 * ship placement information, while hiding their opponent's terrain info
*)
let display_gamestate gstate plyr own =
  (* Determine which player's board to display *)
  let brd = (
    match plyr, own with
    | Player1, true
    | Player2, false ->
                    (fst gstate).board
    | Player1, false
    | Player2, true -> (snd gstate).board
  ) in
  (* Helper function for displaying a row's tilestates *)
  let display_row rw =
    List.fold_left (fun acc r ->
      match r with
      | (Water, Empty) ->   acc ^ "-"
      | (Water, Miss) ->    acc ^ "o"
      | (ShipPart s, Hit) ->    acc ^ "X"
      | (ShipPart s, Empty) ->  acc ^ (if own then "#" else "-")
      | (_, _) ->           acc ^ "?"
    ) "" rw in
  (* Return a display result in the form of a string for each row *)
  List.fold_left (fun result row ->
    result ^ display_row row ^ "\n"
  ) "" brd


(* -----------------------------------------------------------------------------
 * Game State Functions - Victory
----------------------------------------------------------------------------- *)

(** Returns: TODO
 * TODO: Implementation spec/comments
*)
let victory (gs : gamestate) : player option =
  let unhit_in_row rw : int =
    List.fold_left (fun acc r ->
      match r with
      | (ShipPart s, Empty) -> acc + 1
      | (_,_) -> acc
    ) 0 rw
  in
  let vic_on_board (b : grid) : bool =
    let result = List.fold_left (fun acc r ->
      acc + (unhit_in_row r)
    ) 0 b in
    result = 0
  in
  if vic_on_board (fst gs).board then Some (Player2)
  else if vic_on_board (snd gs).board then Some (Player1)
  else None