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
 * Game State Functions - Ship Length and String
----------------------------------------------------------------------------- *)

(** Returns the length of a ship as an integer *)
let ship_length = function
 | Jetski -> 1
 | Patrol -> 2
 | Cruiser -> 3
 | Submarine -> 4
 | Battleship -> 5
 | Carrier -> 6

(* Converts a ship type to its string value *)
let ship_string = function
 | Jetski -> "Jetski"
 | Patrol -> "Patrol"
 | Cruiser -> "Cruiser"
 | Submarine -> "Submarine"
 | Battleship -> "Battleship"
 | Carrier -> "Carrier"

(* -----------------------------------------------------------------------------
 * Game State Functions - Turn
----------------------------------------------------------------------------- *)

(* Given a char, returns an int representing the corresponding row.
 *     e.g. a -> 0, b-> 1, etc.
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


(** Returns: a pair of the tilestate and new gamestate after a move has been
 *    made.
 *  If the coordinate is out of bounds, the tilestate is None; if the tile has
 *    already been picked, the tilestate is Some Empty. In both cases, the
 *    gamestate is returned unchanged.
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
      | _ -> (Some Empty, s)
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

(* Returns: true if the ship at the current coordinate and direction would
 *  be out of bounds on the board
 * Pre: None
*)
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

(* Returns: true if there already exists a ship at the coordinate
 * Pre: The coordinate is within range
*)
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

(* Replace the tiles on the grid with ship parts *)
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
let display_gamestate gstate plyr own formatted =

  (* Helper function to generate the header row of integers from 0 to n.
   * Contains two spaces in the front for proper alignment. *)
  let rec display_hrow n hrow_str =
    match n with
    | -1 -> "    " ^ hrow_str ^ "\n\n"
    | _ -> display_hrow (n-1) ((string_of_int n) ^ " " ^ hrow_str)
  in

  (* Helper function for displaying a row's tilestates *)
  let display_row rw =
    List.fold_left (fun acc r ->
      match r with
      | (Water, Empty) ->
          acc ^ "-" ^ (if formatted then " " else "")

      | (Water, Miss) ->
          acc ^ "o" ^ (if formatted then " " else "")

      | (ShipPart s, Hit) ->
          acc ^ "X" ^ (if formatted then " " else "")

      | (ShipPart s, Empty) ->
          acc ^ (if own then "#" else "-") ^ (if formatted then " " else "")

      | (_, _) ->
          acc ^ "?" ^ (if formatted then " " else "")

    ) "" rw in

  (* Helper function for displaying a game side, formatted with letters in front
   * of the rows *)
  let rec display_result brows n stracc =
    match brows with
    | [] -> stracc
    | h::tl ->
      let letter =
        if formatted
        then (String.make 1 (char_of_int n) ^ "   ")
        else ""
      in
      display_result tl (n + 1) (stracc ^ letter ^ (display_row h) ^ "\n")
  in

  (* Determine which player's board to display *)
  let brd = (
    match plyr, own with
    | Player1, true
    | Player2, false ->
                    (fst gstate).board
    | Player1, false
    | Player2, true -> (snd gstate).board
  ) in

  let gsize = List.length brd in
  let hrow = if formatted then display_hrow (gsize - 1) "" else "" in


  (* Return a display result in the form of a string for each row *)
  display_result brd 65 hrow


(* -----------------------------------------------------------------------------
 * Game State Functions - Victory
----------------------------------------------------------------------------- *)

(* Returns Some [the player that won] if there are no ships remaining on
 * the board that have not been destroyed signaling the end of the game,
 * None if the game has not been won yet
 * Precondition: Each side in the gamestate contains a grid that is not empty*)
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