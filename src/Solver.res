open Types

let sudoku: sudoku = [
  0,
  0,
  5,
  0,
  0,
  0,
  8,
  0,
  0,
  0,
  8,
  0,
  4,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  2,
  0,
  0,
  8,
  5,
  0,
  7,
  9,
  0,
  0,
  3,
  6,
  0,
  0,
  0,
  2,
  0,
  1,
  0,
  0,
  9,
  0,
  0,
  0,
  0,
  4,
  0,
  0,
  5,
  0,
  0,
  0,
  3,
  0,
  0,
  7,
  0,
  0,
  0,
  2,
  0,
  0,
  1,
  0,
  0,
  9,
  0,
  7,
  0,
  0,
  6,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
]

if sudoku->Array.length !== 81 {
  raise(Failure("Invalid sudoku"))
}

let rec easyStrategies = sudoku => {
  let originalSudoku = [...sudoku]
  let sudoku =
    sudoku
    ->Strategies.nakedSingle
    ->Strategies.hiddenSingle

  if originalSudoku == sudoku {
    sudoku
  } else {
    easyStrategies(sudoku)
  }
}

let rec mediumStrategies = sudoku => {
  let originalSudoku = [...sudoku]
  let sudoku =
    sudoku
    ->Strategies.intersection
    ->Strategies.nakedPair
    ->Strategies.hiddenPair

  if originalSudoku == sudoku {
    sudoku
  } else {
    mediumStrategies(sudoku)
  }
}

sudoku
->easyStrategies
->mediumStrategies
->Utilities.toRows
->Js.log
