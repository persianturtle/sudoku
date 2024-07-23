open Types

/**
 If there is only one possibility for a cell, fill it in.

 https://hodoku.sourceforge.net/en/tech_singles.php#n1
 */
let nakedSingle = (sudoku, ~candidatesMap=Utilities.toCandidates(sudoku)) => {
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

 https://hodoku.sourceforge.net/en/tech_singles.php#h1
 */
let hiddenSingle = (sudoku, ~candidatesMap=Utilities.toCandidates(sudoku)) => {
  candidatesMap
  ->Map.entries
  ->Iterator.toArray
  ->Array.forEach(((key, _)) => {
    let rowIndexes = Utilities.getRowKeysForKey(key)
    let columnIndexes = Utilities.getColumnKeysForKey(key)
    let boxIndexes = Utilities.getBoxKeysForKey(key)

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

/**
 If in a block all candidates of a certain digit are confined to a row or column,
 that digit cannot appear outside of that block in that row or column.

 https://hodoku.sourceforge.net/en/tech_intersections.php
 */
let intersection = (sudoku, ~candidatesMap=Utilities.toCandidates(sudoku)) => {
  candidatesMap
  ->Map.entries
  ->Iterator.toArray
  ->Array.forEach(((key, _)) => {
    let rowIndexes = Utilities.getRowKeysForKey(key)
    let columnIndexes = Utilities.getColumnKeysForKey(key)
    let boxIndexes = Utilities.getBoxKeysForKey(key)
    let boxCandidates = Utilities.getUnitCandidatesForKeys(~keys=boxIndexes, ~candidatesMap)

    let getCandidatesThatMustExistInBox = () => {
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

      let isCandidateUniqueToRowOrColumn = (currentBoxRowOrColumn, boxCandidates, candidate) => {
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

      let currentBoxRow = Utilities.floor(boxIndexes->Array.indexOf(key) / 3)
      let currentBoxColumn = mod(boxIndexes->Array.indexOf(key), 3)

      [1, 2, 3, 4, 5, 6, 7, 8, 9]->Array.reduce(([], []), (
        (uniqueRowCandidates, uniqueColumnCandidates),
        candidate,
      ) => {
        let isUniqueRowCandidate = isCandidateUniqueToRowOrColumn(
          currentBoxRow,
          boxRowCandidates,
          candidate,
        )
        let isUniqueColumnCandidate = isCandidateUniqueToRowOrColumn(
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

    let (
      rowCandidatesToRemoveOutsideOfBox,
      columnCandidatesToRemoveOutsideOfBox,
    ) = getCandidatesThatMustExistInBox()

    // Remove locked candidates in row
    rowIndexes
    ->Array.filter(index => !Array.includes(boxIndexes, index))
    ->Array.forEach(index => {
      rowCandidatesToRemoveOutsideOfBox->Array.forEach(
        candidate => {
          Utilities.removeCandidate(~key=index, ~candidate, ~candidatesMap)
        },
      )
    })

    // Remove locked candidates in column
    columnIndexes
    ->Array.filter(index => !Array.includes(boxIndexes, index))
    ->Array.forEach(index => {
      columnCandidatesToRemoveOutsideOfBox->Array.forEach(
        candidate => {
          Utilities.removeCandidate(~key=index, ~candidate, ~candidatesMap)
        },
      )
    })
  })

  sudoku->nakedSingle(~candidatesMap)->hiddenSingle(~candidatesMap)
}

/**
 If two cells, both in the same unit, that have only the same
 two candidates left, those two candidates can be eliminated from all other cells in that unit.

 https://hodoku.sourceforge.net/en/tech_naked.php#n2n2
 */
let nakedPair = (sudoku, ~candidatesMap=Utilities.toCandidates(sudoku)) => {
  candidatesMap
  ->Map.entries
  ->Iterator.toArray
  ->Array.forEach(((key, _)) => {
    let rowIndexes = Utilities.getRowKeysForKey(key)
    let columnIndexes = Utilities.getColumnKeysForKey(key)
    let boxIndexes = Utilities.getBoxKeysForKey(key)

    let getPairs = candidates =>
      candidates->Array.filterWithIndex(({unitCandidates}, index) =>
        unitCandidates->Array.length === 2 &&
          candidates->Array.someWithIndex(
            ({unitCandidates: candidates}, i) => index !== i && unitCandidates == candidates,
          )
      )

    let removePairCandidatesFromRemainderOfUnit = (unitIndexes, pairs) => {
      pairs->Array.forEach(({unitCandidates}) => {
        unitIndexes->Array.forEach(
          index => {
            let candidatesForIndex = candidatesMap->Map.get(index)->Option.getOr([])

            if candidatesForIndex != unitCandidates && candidatesForIndex->Array.length > 0 {
              unitCandidates->Array.forEach(
                candidate => {
                  Utilities.removeCandidate(~key=index, ~candidate, ~candidatesMap)
                },
              )
            }
          },
        )
      })
    }

    let rowCandidates = Utilities.getUnitCandidatesForKeys(~keys=rowIndexes, ~candidatesMap)
    let columnCandidates = Utilities.getUnitCandidatesForKeys(~keys=columnIndexes, ~candidatesMap)
    let boxCandidates = Utilities.getUnitCandidatesForKeys(~keys=boxIndexes, ~candidatesMap)

    let pairsInRow = getPairs(rowCandidates)
    let pairsInColumn = getPairs(columnCandidates)
    let pairsInBox = getPairs(boxCandidates)

    // If a unit contains pairs, remove those candidates from the rest of the unit
    removePairCandidatesFromRemainderOfUnit(rowIndexes, pairsInRow)
    removePairCandidatesFromRemainderOfUnit(columnIndexes, pairsInColumn)
    removePairCandidatesFromRemainderOfUnit(boxIndexes, pairsInBox)
  })

  sudoku->intersection(~candidatesMap)->nakedSingle(~candidatesMap)->hiddenSingle(~candidatesMap)
}

/**
 If two cells are within a unit such that two candidates appear no where
 outside those cells in that unit, those two candidates must be placed in those two cells
 and all other candidates can therefore be eliminated within those hidden pairs.

 https://hodoku.sourceforge.net/en/tech_hidden.php#h2
 */
let hiddenPair = (sudoku, ~candidatesMap=Utilities.toCandidates(sudoku)) => {
  candidatesMap
  ->Map.entries
  ->Iterator.toArray
  ->Array.forEach(((key, _)) => {
    let rowIndexes = Utilities.getRowKeysForKey(key)
    let columnIndexes = Utilities.getColumnKeysForKey(key)
    let boxIndexes = Utilities.getBoxKeysForKey(key)

    let getHiddenPairs = candidates => {
      let map: Map.t<int, int> = Map.make()

      // Count the occurence of each candidate in the unit
      candidates
      ->Array.flatMap(({unitCandidates}) => unitCandidates)
      ->Array.forEach(candidate => {
        map->Map.set(candidate, map->Map.get(candidate)->Option.getOr(0) + 1)
      })

      // Find the candidates that appear exactly twice in the unit
      let candidatesAppearingExactlyTwiceInUnit =
        map
        ->Map.entries
        ->Iterator.toArray
        ->Array.filter(((_, count)) => count === 2)
        ->Array.map(((key, _)) => key)

      // Find the hidden pairs by filtering on the unitCandidates that contain exactly two candidates
      // that each appear exactly twice in the unit
      let unitCandidatesContainingAtTwoCandidatesThatEachAppearExactlyTwiceInUnit =
        candidates->Array.filter(({unitCandidates}) => {
          let candidatesInUnit =
            unitCandidates->Array.filter(
              candidate => candidatesAppearingExactlyTwiceInUnit->Array.includes(candidate),
            )

          candidatesInUnit->Array.length == 2
        })

      // Return the hidden pairs (when there are exactly two) along with the candidates that are part of the hidden pair
      if (
        Array.length(unitCandidatesContainingAtTwoCandidatesThatEachAppearExactlyTwiceInUnit) === 2
      ) {
        Some(
          unitCandidatesContainingAtTwoCandidatesThatEachAppearExactlyTwiceInUnit,
          candidatesAppearingExactlyTwiceInUnit->Array.filter(candidate => {
            unitCandidatesContainingAtTwoCandidatesThatEachAppearExactlyTwiceInUnit
            ->Array.flatMap(({unitCandidates}) => unitCandidates)
            ->Array.includes(candidate)
          }),
        )
      } else {
        None
      }
    }

    let removeOtherCandidatesFromHiddenPair = (hiddenPairs, candidatesInPair) => {
      hiddenPairs->Array.forEach(({key}) => {
        candidatesMap->Map.set(
          key,
          candidatesMap
          ->Map.get(key)
          ->Option.getExn
          ->Array.filter(candidate => Array.includes(candidatesInPair, candidate)),
        )
      })
    }

    let rowCandidates = Utilities.getUnitCandidatesForKeys(~keys=rowIndexes, ~candidatesMap)
    let columnCandidates = Utilities.getUnitCandidatesForKeys(~keys=columnIndexes, ~candidatesMap)
    let boxCandidates = Utilities.getUnitCandidatesForKeys(~keys=boxIndexes, ~candidatesMap)

    // If a unit contains hidden pairs, remove the remaining candidates from the hidden pairs
    switch getHiddenPairs(rowCandidates) {
    | None => ()
    | Some((hiddenPairsInRow, candidatesInPair)) =>
      removeOtherCandidatesFromHiddenPair(hiddenPairsInRow, candidatesInPair)
    }
    switch getHiddenPairs(columnCandidates) {
    | None => ()
    | Some((hiddenPairsInColumn, candidatesInPair)) =>
      removeOtherCandidatesFromHiddenPair(hiddenPairsInColumn, candidatesInPair)
    }
    switch getHiddenPairs(boxCandidates) {
    | None => ()
    | Some((hiddenPairsInBox, candidatesInPair)) =>
      removeOtherCandidatesFromHiddenPair(hiddenPairsInBox, candidatesInPair)
    }
  })

  sudoku
  ->nakedPair(~candidatesMap)
  ->intersection(~candidatesMap)
  ->nakedSingle(~candidatesMap)
  ->hiddenSingle(~candidatesMap)
}
