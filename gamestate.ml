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
(* We'll probably want a function that..
 *   (1) Checks if the coordinate is out of the grid's range
 *   (2) Translates coordinates to indices
 *)

let get_row c = (Char.code (Char.uppercase c)) - (Char.code 'A')

(* Precondition: crd is a valid coordinate *)
let replace_element g ts crd : grid =
  let rowNum = get_row (fst crd) in
  let colNum = snd crd in
  let rec replace_in_row row acc: (terrain * tilestate) list =
    match row with
    | [] -> []
    | h::t -> if acc = colNum
              then
                let (a,_) = h in
                (a,ts) :: (replace_in_row t (acc+1))
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

(*Currently can handle lowercase or uppercase coords*)
(** Returns: a pair of the tilestate and new gamestate after a move has been
 *    made. If the coordinate is not a valid tile or it has already been picked,
 *    then tilestate is Empty and the gamestate is returned unchanged.
 *  Precondition: Player is the player that is making the move.
 *)
let turn gstate crd plyr : (tilestate * gamestate) =
  let makeMove s c =
    let g = s.board in
    try
      let row = List.nth g (get_row (fst c)) in
      let tile = List.nth row (snd c) in
      begin match tile with
      | (Water, Empty) -> let newg = replace_element g Miss crd in
                          let news = {board = newg; ships = s.ships} in
                          (Miss, news)
      | (ShipPart _, Empty) -> let newg = replace_element g Miss crd in
                               let news = {board = newg; ships = s.ships} in
                               (Hit, news)
      | _ -> raise (Failure "Already hit")
      end
    with
      _ -> (Empty, s)
  in
  match plyr, gstate with
  | Player1 x, (s1,s2) -> let (a,b) = makeMove s2 crd in (a, (s1,b))
  | Player2 x, (s1,s2) -> let (a,b) = makeMove s1 crd in (a, (b,s2))

(* -----------------------------------------------------------------------------
 * Game State Functions - Victory
----------------------------------------------------------------------------- *)

(** Returns: TODO
 * TODO: Implementation spec/comments
*)
(*let victory gstate : (player option) =
  failwith "TODO - victory"*)


(* -----------------------------------------------------------------------------
 * Game State Functions - Place Ship
----------------------------------------------------------------------------- *)

(** Returns: TODO
 * TODO: Implementation spec/comments
*)
(*let place_ship gstate shp crd dr : (gamestate) =
  failwith "TODO - place_ship"*)
let place_ship (side : side) (ship : ship)
                  (c : coord) (d : dir) : side =
  failwith "must implement"


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