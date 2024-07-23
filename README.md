# Sudoku Solver

The goal of this sudoku solver is to define various strategies that I've been learning about [here](https://hodoku.sourceforge.net/en/tech_intro.php).

The strategies that this sudoku solver supports can be found in [Strategies.res](src/Strategies.res).

The input to the program (i.e. the unsolved sudoku) is hard coded in [Solver.res](src/Solver.res). Sudukus are represented by a flat array of numbers, where `0` represents the absence of a number.

To customize the strategies being applied when solving, edit [Solver.res](src/Solver.res).

```
sudoku
->easyStrategies
->mediumStrategies
->hardStrategies
->Utilities.toRows
->Js.log
```

## Installation

```sh
npm install
```

## Build

- Build: `npm run res:build`
- Clean: `npm run res:clean`
- Build & watch: `npm run res:dev`

## Run

```sh
npm run solve
```
