(*Game state file*)

open Gamestate

(** Recursive user text interaction for game after game has been initialized *)
val repl : gamestate -> playerstate -> bool -> bool -> unit

(** Prompts player to place all ships and returns the updated gamestate. *)
val place_ships: side -> fleet -> side

(** Takes in a keyword string passed from repl to determine appropriate action.
 *  Returns updated gamestate and true if next player's turn *)
val interp_input: gamestate -> player -> string -> gamestate * bool

(** Main function for playing game. Allows players to initialize grids with ships
  * and starts game with user text interaction *)
val main: unit -> unit