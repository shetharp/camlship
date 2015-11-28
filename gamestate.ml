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

type coord = char * int

type fleet = (ship * coord list) list

type dir = Up | Down | Left | Right

type player = Player1 of string | Player2 of string

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

(** Returns: TODO
 * TODO: Implementation spec/comments
*)
let turn gstate crd plyr : (tilestate * gamestate) =
  failwith "TODO - turn"


(* -----------------------------------------------------------------------------
 * Game State Functions - Victory
----------------------------------------------------------------------------- *)

(** Returns: TODO
 * TODO: Implementation spec/comments
*)
let victory gstate : (player option) =
  failwith "TODO - victory"


(* -----------------------------------------------------------------------------
 * Game State Functions - Place Ship
----------------------------------------------------------------------------- *)

(** Returns: TODO
 * TODO: Implementation spec/comments
*)
let place_ship (side : side) (ship : ship)
                  (c : coord) (d : dir) : side =
  failwith "must implement"
(*let place_ship gstate shp crd dr : (gamestate) =
  failwith "TODO - place_ship"*)


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
let display_gamestate gstate plyr (* own [of type bool] *) =
  (* Remove if own is implemented as an argument and spec is updated*)
  let own = true in

  (* Determine which player's board to display *)
  let brd = (
    match plyr, own with
    | Player1 n, true
    | Player2 n, false ->
                    (fst gstate).board
    | Player1 n, false
    | Player2 n, true -> (snd gstate).board
  ) in
  (* Helper function for displaying a row's tilestates *)
  let display_row rw =
    List.fold_left (fun acc r ->
      match r with
      | (Water, Empty) ->   acc ^ "-"
      | (Water, Miss) ->    acc ^ "o"
      | (ShipPart s, Hit) ->    acc ^ "X"
      | (ShipPart s, Empty) ->  acc ^ "#"
      | (_, _) ->           acc ^ "?"
    ) "" rw in
  (* Return a display result in the form of a string for each row *)
  List.fold_left (fun result row ->
    result ^ display_row row ^ "\n"
  ) "" brd

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
  if vic_on_board (fst gs).board then Some (Player2("player2"))
  else if vic_on_board (snd gs).board then Some (Player1("player1"))
  else None