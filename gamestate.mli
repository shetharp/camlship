(* Size of the grid. So if GRID_SIZE is 8 then the grid is 8 by 8 tiles *)
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
 * 1 is the leftmost, 2 is the next column, and so on*)
type coord = char * int

(* List of ships along with their type and the number of remaining unhit
 * pieces that that ship. *)
type fleet = ship list

(* Directions that the ship can placed*)
type dir = Up | Down | Left | Right

type player = Player1 | Player2

type playerstate = {first : string; second : string; current : player}

type side = {board : grid; ships : fleet}

type gamestate = side * side

val ship_length : ship -> int

(* Returns a new grid with the tilestate updated at that coord and that
 * new tilestate passed back in the tuple as an option. If coord is out of range
 * or already played, then return None for the tilestate option and return the
 * original grid. *)
val turn : gamestate -> coord -> player -> (tilestate option * gamestate)

(* Returns true if there are no ships remaining on the board that have
 * not been destroyed.
 * Precondition: Each side in the gamestate contains a grid that is not empty*)
val victory : gamestate -> player option

(* Prints out the grid while clearly displaying
 * the terrain and action for each tile
 * Prints out the grid displaying only the action associated with each tile
 * and not showing the terrain **)
val display_gamestate: gamestate -> player -> bool -> string

(* Places the ship on the grid starting at the coord given and in the
 * direction on the board. Returns a new grid and the players fleet updated
 * with the ship. Returns None if coord is out of range or
 * overlapping with a current ship. *)
val place_ship : side -> ship -> coord -> dir -> side option

