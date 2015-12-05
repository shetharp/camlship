open Gamestate

(* Intitializes the grid with a strategic placement of ships.*)
val ai_place_ships: side -> fleet -> side

(* If on easy mode, returns a random coordinate. If not on easy mode, returns
 * the best coordinate (based on previous hits/misses).
 *)
val make_move: grid -> bool -> coord
