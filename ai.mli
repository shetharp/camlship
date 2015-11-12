open Grid

(* Intitializes the grid with randomly placed ships that do not touch. *)
val ai_place_ships: unit -> grid * fleet

(* Returns the best tile to hit.
 *    - If it knows of a hit on the grid that is not a sunken ship then
 *      it will choose a spot adjacent to that hit
 *    - If there are no known hits then it will randomly choose a spot
 *      at least two tiles away *)
val best_move : action list list -> coord