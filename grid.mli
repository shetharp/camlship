
val GRID_SIZE : int

type ship =
  | Jetski
  | Patrol
  | Cruiser
  | Submarine
  | Battleship
  | Carrier

type terrain = Water | ShipPart of ship

type action = Hit | Miss | None

type grid = (terrain * action) list list

type coord = char * int

val turn : grid -> coord -> grid

val continue_game : grid -> bool


type fleet = (ship * coord list) list

type dir = Up | Down | Left | Right

val place_ship : grid -> fleet -> ship -> coord -> dir -> (grid * fleet)

