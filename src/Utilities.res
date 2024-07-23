open Types

let floor = n => Math.floor(n->Int.toFloat)->Float.toInt

let toRows = (sudoku): rows => {
  let indexes = [0, 1, 2, 3, 4, 5, 6, 7, 8]->Array.map(n => n * 9)
  indexes->Array.mapWithIndex((start, end) =>
    switch indexes[end + 1] {
    | None => sudoku->Array.sliceToEnd(~start)
    | Some(end) => sudoku->Array.slice(~start, ~end)
    }
  )
}

let toColumns = (sudoku): columns => {
  let indexes = [0, 1, 2, 3, 4, 5, 6, 7, 8]
  indexes->Array.map(columnIndex =>
    sudoku->Array.filterWithIndex((_, index) => mod(index, 9) === columnIndex)
  )
}

let toBoxes = (sudoku): boxes => {
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

let toCandidates = sudoku => {
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

let getUnitCandidatesForKeys = (~keys, ~candidatesMap) =>
  keys
  ->Array.map(index => candidatesMap->Map.get(index)->Option.getOr([]))
  ->Array.mapWithIndex((candidates, index) => {
    unitCandidates: candidates,
    index,
    key: keys[index]->Option.getExn,
  })

let removeCandidate = (~key, ~candidate, ~candidatesMap) => {
  let candidates = candidatesMap->Map.get(key)->Option.getOr([])
  if candidates->Array.length > 0 {
    candidatesMap->Map.set(key, candidates->Array.filter(c => c !== candidate))
  }
}
