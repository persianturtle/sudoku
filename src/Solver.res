type sudoku = array<int>
type rows = array<array<int>>
type columns = array<array<int>>
type boxes = array<array<int>>
type candidates = Map.t<int, array<int>>

let sudoku: sudoku = [
  0,
  0,
  1,
  2,
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
  0,
  0,
  0,
  1,
  0,
  7,
  0,
  0,
  5,
  0,
  8,
  0,
  0,
  0,
  3,
  0,
  0,
  7,
  2,
  6,
  0,
  0,
  8,
  1,
  0,
  0,
  6,
  0,
  0,
  5,
  0,
  0,
  0,
  4,
  0,
  0,
  0,
  0,
  0,
  2,
  6,
  4,
  7,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  8,
  0,
  0,
  4,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  3,
  7,
  0,
  0,
  9,
]

if sudoku->Array.length !== 81 {
  raise(Failure("Invalid sudoku"))
}

let floor = n => Math.floor(n->Int.toFloat)->Float.toInt

let toRows = (sudoku: sudoku): rows => {
  let indexes = [0, 1, 2, 3, 4, 5, 6, 7, 8]->Array.map(n => n * 9)
  indexes->Array.mapWithIndex((start, end) =>
    switch indexes[end + 1] {
    | None => sudoku->Array.sliceToEnd(~start)
    | Some(end) => sudoku->Array.slice(~start, ~end)
    }
  )
}

let toColumns = (sudoku: sudoku): columns => {
  let indexes = [0, 1, 2, 3, 4, 5, 6, 7, 8]
  indexes->Array.map(columnIndex =>
    sudoku->Array.filterWithIndex((_, index) => mod(index, 9) === columnIndex)
  )
}

let toBoxes = (sudoku: sudoku): boxes => {
  sudoku->Array.reduceWithIndex([], (boxes: boxes, value: int, key) => {
    let row = floor(key / 9)
    let column = mod(key, 9)
    let boxIndex = floor(row / 3) * 3 + floor(column / 3)

    boxes[boxIndex] = switch boxes[boxIndex] {
    | None => [value]
    | Some(box) =>
      box->Array.push(value)
      box
    }
    boxes
  })
}

let getRowKeysForKey = key => {
  let key = floor(key / 9) * 9
  [key, key + 1, key + 2, key + 3, key + 4, key + 5, key + 6, key + 7, key + 8]
}

let getColumnKeysForKey = key => {
  let key = mod(key, 9)
  [key, key + 9, key + 18, key + 27, key + 36, key + 45, key + 54, key + 63, key + 72]
}

let getBoxKeysForKey = key => {
  let rowIndex = floor(key / 9)
  let columnIndex = mod(key, 9)
  let boxIndex = floor(rowIndex / 3) * 3 + floor(columnIndex / 3)
  let key = floor(boxIndex / 3) * 27 + mod(boxIndex, 3) * 3
  [key, key + 1, key + 2, key + 9, key + 10, key + 11, key + 18, key + 19, key + 20]
}

let toCandidates = (sudoku: sudoku) => {
  let candidates = Map.make()
  let rows = toRows(sudoku)
  let columns = toColumns(sudoku)
  let boxes = toBoxes(sudoku)

  sudoku->Array.forEachWithIndex((value, key) => {
    if value === 0 {
      candidates->Map.set(
        key,
        [1, 2, 3, 4, 5, 6, 7, 8, 9]->Array.filter(value => {
          let rowIndex = floor(key / 9)
          let columnIndex = mod(key, 9)
          let boxIndex = floor(rowIndex / 3) * 3 + floor(columnIndex / 3)

          let row = rows[rowIndex]->Option.getExn
          let column = columns[columnIndex]->Option.getExn
          let box = boxes[boxIndex]->Option.getExn

          !(
            Array.includes(row, value) ||
            Array.includes(column, value) ||
            Array.includes(box, value)
          )
        }),
      )
    }
  })

  candidates
}

/**
 If there is only one possibility for a cell, fill it in.
 */
let nakedSingleStrategy = (sudoku, ~candidatesMap=toCandidates(sudoku)) => {
  candidatesMap
  ->Map.entries
  ->Iterator.toArray
  ->Array.forEach(((key, possibleValues)) => {
    if possibleValues->Array.length === 1 {
      sudoku[key] = possibleValues[0]->Option.getExn
    }
  })

  sudoku
}

/**
 If the row, column, or box candidates have only one cell that can contain a certain value,
 even if there are multiple candidates for that cell, fill it in.
 */
let nakedCandidateStrategy = (sudoku, ~candidatesMap=toCandidates(sudoku)) => {
  candidatesMap
  ->Map.entries
  ->Iterator.toArray
  ->Array.forEach(((key, _)) => {
    let rowIndexes = getRowKeysForKey(key)
    let columnIndexes = getColumnKeysForKey(key)
    let boxIndexes = getBoxKeysForKey(key)

    let rowCandidates = rowIndexes->Array.map(key => candidatesMap->Map.get(key)->Option.getOr([]))
    let columnCandidates =
      columnIndexes->Array.map(key => candidatesMap->Map.get(key)->Option.getOr([]))
    let boxCandidates = boxIndexes->Array.map(key => candidatesMap->Map.get(key)->Option.getOr([]))

    [1, 2, 3, 4, 5, 6, 7, 8, 9]->Array.forEach(value => {
      if rowCandidates->Array.flat->Array.filter(v => v === value)->Array.length === 1 {
        let index = rowCandidates->Array.findIndex(candidates => candidates->Array.includes(value))

        sudoku[rowIndexes[index]->Option.getExn] = value
      }
      if columnCandidates->Array.flat->Array.filter(v => v === value)->Array.length === 1 {
        let index =
          columnCandidates->Array.findIndex(candidates => candidates->Array.includes(value))

        sudoku[columnIndexes[index]->Option.getExn] = value
      }
      if boxCandidates->Array.flat->Array.filter(v => v === value)->Array.length === 1 {
        let index = boxCandidates->Array.findIndex(candidates => candidates->Array.includes(value))

        sudoku[boxIndexes[index]->Option.getExn] = value
      }
    })
  })

  sudoku
}

/* Index refers tothe index within a row, column, or box. Key refers to the index of the entire sudoku array */
type unitCandidates = {unitCandidates: array<int>, index: int, key: int}

/**
 When we find pairs or triples within a row, column, or box, we can remove those numbers
 from the rest of the row, column, or box. Pairs are candidates that only have two possibilities,
 and both pairs must exist in the same unit (i.e. row, column, or box) to be considered a pair.
 Similarly, triples are candidates that only have three possibilities, and all three must exist
 in the same unit.
 */
let intersectionRemovalStrategy = sudoku => {
  let candidatesMap = toCandidates(sudoku)

  candidatesMap
  ->Map.entries
  ->Iterator.toArray
  ->Array.forEach(((key, _)) => {
    let rowIndexes = getRowKeysForKey(key)
    let columnIndexes = getColumnKeysForKey(key)
    let boxIndexes = getBoxKeysForKey(key)

    let getCandidates = indexes =>
      indexes
      ->Array.map(index => candidatesMap->Map.get(index)->Option.getOr([]))
      ->Array.mapWithIndex((candidates, index) => {
        unitCandidates: candidates,
        index,
        key: boxIndexes[index]->Option.getExn,
      })

    let getPairs = candidates =>
      candidates->Array.filterWithIndex(({unitCandidates}, index) =>
        unitCandidates->Array.length === 2 &&
          candidates->Array.someWithIndex(
            ({unitCandidates: candidates}, i) => index !== i && unitCandidates == candidates,
          )
      )

    let removeCandidate = (key, candidate) => {
      let candidates = candidatesMap->Map.get(key)->Option.getOr([])
      candidatesMap->Map.set(key, candidates->Array.filter(c => c !== candidate))
    }

    let getCandidatesThatMustExistInBox = (boxIndexes, boxCandidates) => {
      let candidates = boxCandidates->Array.map(({unitCandidates}) => unitCandidates)

      let boxRowCandidates = {
        let indexes = [0, 1, 2]->Array.map(n => n * 3)
        indexes->Array.mapWithIndex((start, end) =>
          switch indexes[end + 1] {
          | None => candidates->Array.sliceToEnd(~start)->Array.flat
          | Some(end) => candidates->Array.slice(~start, ~end)->Array.flat
          }
        )
      }

      let boxColumnCandidates = {
        let indexes = [0, 1, 2]
        indexes->Array.map(columnIndex =>
          candidates->Array.filterWithIndex((_, index) => mod(index, 3) === columnIndex)->Array.flat
        )
      }

      let isUniqueCandidate = (currentBoxRowOrColumn, boxCandidates, candidate) => {
        switch currentBoxRowOrColumn {
        | 0 =>
          boxCandidates[0]->Option.getExn->Array.includes(candidate) &&
          !(boxCandidates[1]->Option.getExn->Array.includes(candidate)) &&
          !(boxCandidates[2]->Option.getExn->Array.includes(candidate))
        | 1 =>
          boxCandidates[1]->Option.getExn->Array.includes(candidate) &&
          !(boxCandidates[0]->Option.getExn->Array.includes(candidate)) &&
          !(boxCandidates[2]->Option.getExn->Array.includes(candidate))
        | 2 =>
          boxCandidates[2]->Option.getExn->Array.includes(candidate) &&
          !(boxCandidates[0]->Option.getExn->Array.includes(candidate)) &&
          !(boxCandidates[1]->Option.getExn->Array.includes(candidate))
        | _ => assert(false)
        }
      }

      let currentBoxRow = floor(boxIndexes->Array.indexOf(key) / 3)
      let currentBoxColumn = mod(boxIndexes->Array.indexOf(key), 3)

      [1, 2, 3, 4, 5, 6, 7, 8, 9]->Array.reduce(([], []), (
        (uniqueRowCandidates, uniqueColumnCandidates),
        candidate,
      ) => {
        let isUniqueRowCandidate = isUniqueCandidate(currentBoxRow, boxRowCandidates, candidate)
        let isUniqueColumnCandidate = isUniqueCandidate(
          currentBoxColumn,
          boxColumnCandidates,
          candidate,
        )

        (
          isUniqueRowCandidate ? [...uniqueRowCandidates, candidate] : uniqueRowCandidates,
          isUniqueColumnCandidate ? [...uniqueColumnCandidates, candidate] : uniqueColumnCandidates,
        )
      })
    }

    let removePairCandidatesFromRemainderOfUnit = (unitIndexes, pairs) => {
      pairs->Array.forEach(({unitCandidates}) => {
        unitIndexes->Array.forEach(
          index => {
            let candidatesForIndex = candidatesMap->Map.get(index)->Option.getOr([])

            if candidatesForIndex != unitCandidates && candidatesForIndex->Array.length > 0 {
              unitCandidates->Array.forEach(
                candidate => {
                  removeCandidate(index, candidate)
                },
              )
            }
          },
        )
      })
    }

    let rowCandidates = getCandidates(rowIndexes)
    let columnCandidates = getCandidates(columnIndexes)
    let boxCandidates = getCandidates(boxIndexes)

    let pairsInRow = getPairs(rowCandidates)
    let pairsInColumn = getPairs(columnCandidates)
    let pairsInBox = getPairs(boxCandidates)

    // If a unit contains pairs, remove those candidates from the rest of the unit
    removePairCandidatesFromRemainderOfUnit(rowIndexes, pairsInRow)
    removePairCandidatesFromRemainderOfUnit(columnIndexes, pairsInColumn)
    removePairCandidatesFromRemainderOfUnit(boxIndexes, pairsInBox)

    let (
      rowCandidatesToRemoveOutsideOfBox,
      columnCandidatesToRemoveOutsideOfBox,
    ) = getCandidatesThatMustExistInBox(boxIndexes, boxCandidates)

    // If a row contains candidates that must exist in a box's row,
    // remove those candidates from the rest of the row
    rowIndexes
    ->Array.filter(index => !Array.includes(boxIndexes, index))
    ->Array.forEach(index => {
      rowCandidatesToRemoveOutsideOfBox->Array.forEach(
        candidate => {
          removeCandidate(index, candidate)
        },
      )
    })

    // If a column contains candidates that must exist in a box's column,
    // remove those candidates from the rest of the column
    boxIndexes
    ->Array.filter(index => !Array.includes(boxIndexes, index))
    ->Array.forEach(index => {
      columnCandidatesToRemoveOutsideOfBox->Array.forEach(
        candidate => {
          removeCandidate(index, candidate)
        },
      )
    })
  })

  sudoku->nakedSingleStrategy(~candidatesMap)->nakedCandidateStrategy(~candidatesMap)
}

let rec easyStrategies = sudoku => {
  let originalSudoku = [...sudoku]
  let sudoku =
    sudoku
    ->nakedSingleStrategy
    ->nakedCandidateStrategy

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
    ->intersectionRemovalStrategy
    ->nakedCandidateStrategy
    ->nakedSingleStrategy

  if originalSudoku == sudoku {
    sudoku
  } else {
    mediumStrategies(sudoku)
  }
}

sudoku
->easyStrategies
->mediumStrategies
->toRows
->Js.log
