(*Game state file*)

open Gamestate

(** Recursive user text interaction for game after game has been initialized *)
val repl : gamestate -> unit

(** Prompts player to place all ships and returns the updated gamestate. *)
val place_ships: side -> ship list -> side

val interp_input: gamestate -> player -> string -> gamestate * bool

(** Main function for playing game. Allows players to initialize grids with ships
  * and starts game with user text interaction *)
val main: unit -> unit