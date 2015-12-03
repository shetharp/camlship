open Gamestate

(* Intitializes the grid with randomly placed ships that do not touch. *)
val ai_place_ships: unit -> grid * fleet

(*We'll want to also have it take in a last move*)

(* If on easy mode, returns a random coordinate. If not on easy mode, returns
 * the best coordinate (based on previous hits/misses).
 *)
val make_move: grid -> bool -> coord
