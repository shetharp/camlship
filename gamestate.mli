(* Size of the grid. So if GRID_SIZE is 8 then the grid is 8 by 8 tiles *)
val GRID_SIZE : int

(* Variant of possible ship types *)
type ship =
  | Jetski
  | Patrol
  | Cruiser
  | Submarine
  | Battleship
  | Carrier

(* Defines if there is a part of a ship on that tile or just water *)
type terrain = Water | Ship of ship

(* If the opponent hasn't looked at this tile it will have type None
 * If the opponent has looked at this tile and it was a ship then
 * it will have type Hit and if water then Miss *)
type action = Hit | Miss | Empty

(* A grid of locations with a terrain and an action associated with that tile*)
type grid = (terrain * action) list list

(* Coordinates of tiles. A character represents a row where 'A' is the top row,
 * B is the next row down, and so on. The integer represents the column where
 * 1 is the leftmost, 2 is the next column, and so on*)
type coord = char * int

(* Returns a new grid option with the action updated at that coord and that
 * new action passed back in the tuple. If coord is out of range then
 * return None for the action option and return the original grid. *)
val turn : grid -> coord -> action option * grid

(* Returns true if there are no ships remaining on the board that have
 * not been destroyed. Thus the game will end *)
val victory : fleet -> bool

(* Prints out the grid while clearly displaying
 * the terrain and action for each tile *)
val display_full: grid -> unit

(* Prints out the grid displaying only the action associated with each tile
 * and not showing the terrain *)
val display_actions: grid -> unit

(* List of ships along with their type and the number of remaining unhit
 * pieces that that ship. *)
type fleet = (ship * int) list

(* Directions that the ship can placed*)
type dir = Up | Down | Left | Right

(* Places the ship on the grid starting at the coord given and in the
 * direction on the board. Returns a new grid and the players fleet updated
 * with the ship. Raises an error if coord is out of range or
 * overlapping with a current ship. *)
val place_ship : grid -> fleet -> ship -> coord -> dir -> (grid * fleet)
