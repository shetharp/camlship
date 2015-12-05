=================================================
		CAMLSHIP README
==================================================

To compile the game files, run the following commands in the terminal.

	cs3110 compile gamestate.ml
	cs3110 compile ai.ml
	cs3110 compile -p str gameplay.ml


To run the game, first compile gameplay with the above instructions.
Then, run the following command in the terminal.
	
	cs3110 run gameplay


To run the testing suite, run the following commands in the terminal.

	cs3110 compile test_state.ml
	cs3110 test test_state


Most settings are configurable through the game's user interface.
However, you may wish to change the grid size for debugging purposes.
This change can easily be made by modifying the int value of
grid_size at the top of gamestate.ml file.
This decision was made because standard battleship games are 10x10 grids,
which is the value we have programmed into the game by default.
The int value of grid_size should be between 4 and 10.
We suggest enlarging the size of your terminal window when playing on larger boards.

All other game play instructions are available through the game user interface.
You can quit the game by typing in "quit" or get help by typing in "help".


Have fun, and we hope you enjoy!!

