(* Size of the grid. So if grid_size is 8 then the grid is 8 by 8 tiles
 * Must be between 4 and 10 inclusive.*)
val grid_size : int

(* Variant of possible ship types *)
type ship =
  | Jetski
  | Patrol
  | Cruiser
  | Submarine
  | Battleship
  | Carrier

(* Defines if there is a part of a ship on that tile or just water *)
type terrain = Water | ShipPart of ship

(* If the opponent hasn't looked at this tile it will have type None
 * If the opponent has looked at this tile and it was a ship then
 * it will have type Hit and if water then Miss *)
type tilestate = Hit | Miss | Empty

(* A grid of locations with a terrain and an action associated with that tile*)
type grid = (terrain * tilestate) list list

(* Coordinates of tiles. A character represents a row where 'A' is the top row,
 * B is the next row down, and so on. The integer represents the column where
 * 0 is the leftmost, 2 is the next column, and so on*)
type coord = char * int

(* List of ship types that have been placed on the board. Generated dynamically
 * based on size of the grid*)
type fleet = ship list

(* Directions that the ship can placed*)
type dir = Up | Down | Left | Right

(* Player1 is the human player. Player2 is the AI.*)
type player = Player1 | Player2

(* Record storing information about the current player state.
 * first is the first player's name. second is the second's player name.
 * current is the active player who is making the move.*)
type playerstate = {first : string; second : string; current : player}

(* A player's ship and board information. *)
type side = {board : grid; ships : fleet}

(* Represents the entire board (both human and AI's sides)*)
type gamestate = side * side

(* Given a ship type, returns the number of tiles that it takes up.*)
val ship_length : ship -> int

(* Give a ship type, returns the ship's name. *)
val ship_string : ship -> string

(* Returns the (terrain * tilestate) pair at the given coord.
 * Precondition: The coord must be valid.
 *)
val get_tile : grid -> coord -> (terrain * tilestate)

(* Returns a new grid with the tilestate updated at that coord and that
 * new tilestate passed back in the tuple as an option. If coord is out of range
 * or already played, then return None for the tilestate option and return the
 * original grid. *)
val turn : gamestate -> coord -> player -> (tilestate option * gamestate)

(* Returns Some [the player that won] if there are no ships remaining on
 * the board that have not been destroyed signaling the end of the game,
 * None if the game has not been won yet
 * Precondition: Each side in the gamestate contains a grid that is not empty*)
val victory : gamestate -> player option

(* Returns a string representing the current gamestate. The function takes in a
 * player and a boolean to decide if its own board should be displayed
 * (showing ship placement) or if the opponent's board should be displayed
 * (showing only the action and not terrain). The final bool input is used for
 * debugging and testing purposes. **)
val display_gamestate: gamestate -> player -> bool -> bool -> string

(* Places the ship on the grid starting at the coord given and in the
 * direction on the board. Returns a new grid and the players fleet updated
 * with the ship. Returns None if coord is out of range or
 * overlapping with a current ship. *)
val place_ship : side -> ship -> coord -> dir -> side option

