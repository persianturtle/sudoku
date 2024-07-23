type sudoku = array<int>
type rows = array<array<int>>
type columns = array<array<int>>
type boxes = array<array<int>>
type candidates = Map.t<int, array<int>>

/*
 Unit refers to either a row, column, or box.
 Index refers to the index within the array representing the unit (0 - 8).
 Key refers to the index of the entire sudoku array (0 - 80).
*/
type unitCandidates = {unitCandidates: array<int>, index: int, key: int}
