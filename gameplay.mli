(*Game state file*)

open Grid

(** Recursive user text interaction for game after game has been initialized *)
val repl : game_state -> unit

(** Prompts player to place all ships and returns the updated gamestate. *)
val place_ships: gamestate -> gamestate

val interp_input: gamestate -> string -> gamestate

(** Main function for playing game. Allows players to initialize grids with ships
  * and starts game with user text interaction *)
val main: unit -> unit