# Hasweeper (Minesweeper in Haskell)

This is a CLI Minesweeper implementation in Haskell. When running the code, new board is generated and the game asks for user input. Player can either flag a tile or reveal a tile by entering R or F, and a coordinate, e.g. <kbd>R 0 5</kbd> or <kbd>F 3 7</kbd>.

The player loses if they reveal a mine and wins when they place 10 flags on the board on tiles that are mines. Restart a game by re-running the build. An example game output for a winning round can be found in [sample_game_output.txt](sample_game_output.txt).

## Installation

1. Clone the repository:
```bash
git clone git@github.com:juakiv/hasweeper.git
```

2. Build the project:
```bash
ghc Minesweeper.hs
```

3. Run the build by running the newly created files. On Windows:
```bash
./Minesweeper
```
> [!NOTE]
> Make sure you have random package installed for `System.Random` module to work.
> `cabal install random`