open Gamestate
open Ai


(** Returns a string representation of the grid given a gamestate and player.
 * Legend:
 * X = successful hit
 * o = miss
 * - = water
 *
 * TODO: In the future, extend functionality to show the player their own
 * ship placement information, while hiding their opponent's terrain info
*)
let display_gamestate gstate plyr =
  (* Determine which player's board to display *)
  let brd = (
    match plyr with
    | Player1 n -> (fst gstate).board
    | Player2 n -> (snd gstate).board
  ) in
  (* Helper function for displaying a row's tilestates *)
  let display_row rw =
    List.fold_left (fun acc r ->
      match r with
      | (Water, Empty) ->   acc ^ "-"
      | (Water, Miss) ->    acc ^ "o"
      | (Ship s, Hit) ->    acc ^ "X"
      | (Ship s, Empty) ->  acc ^ "#"
      | (_, _) ->           acc ^ "?"
    ) "" rw in
  (* Return a display result in the form of a string for each row *)
  List.fold_left (fun result row ->
    result ^ display_row row ^ "\n"
  ) "" brd