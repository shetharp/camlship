(*Game state file*)

open Grid

type player = {name: string; own_grid: grid; own_fleet opponent: player; ai: bool}

type game_state = player * player

(** Checks grid state to determine if game should continue or if game is over.
  * If bool argument is set to false, game is forced to quit.
  *)
val continue_game : grid -> bool -> bool

(** Recursive user text interaction for game after game has been initialized *)
val repl : game_state -> unit

(** Prompts player to place all ships and returns the updated grid and fleet. *)
val place_ships: unit -> (grid*fleet)

(** Main function for playing game. Allows players to initialize grids with ships
  * and starts game with user text interaction *)
val main: unit -> unit