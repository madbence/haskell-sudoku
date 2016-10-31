# haskell-sudoku

## usage

```
> putStrLn $ case solve $ empty 3 of Nothing -> "No solution"; Just sol -> "Solution:\n" ++ showSol sol
```

## explanation

```hs
type Size = Int
type Row = Int
type Col = Int
type Cell = Int
type Value = Int
```

the grid has variable size, where the column/row/cells are labeled with integers.
the grid contains integers.

```
type Loc = (Co, Row, Cell)
type Node = (Loc, [Value])
type Grid = (Size, [Node])
```

every node has an attached location (col/row/cell number), and a list of possible values. the grid has a size, and a list of nodes.

if every node contains only one possible value, the grid is solved.

```hs
fromIndex :: Int -> Int -> Loc
fromIndex i k = (col, row, cell)
  where col = i `mod` k^2
        row = i `quot` k^2
        cell = col `quot` k + (row `quot` k) * k
```

construct `Loc` from numerical index `i`, assuming that the grid has `k^4` nodes.

```hs
empty k = (k, [(fromIndex i k, [1..k^2]) | i <- [0..k^4-1]])
```

construct an empty grid (every field contains all possible values).

```hs
kill k m idx v = [update loc vs | (loc, vs) <- m] where
  (col, row, cell) = fromIndex idx k
  update loc@(col', row', cell') vs | col == col' && row == row' && cell == cell' = (loc, [v])       -- same location, the only possible solution is our candidate
                                    | col /= col' && row /= row' && cell /= cell' = (loc, vs)        -- unrelated location (not same row/col/cell), no change
                                    | otherwise                                   = (loc, vs \\ [v]) -- same row/col/cell, remove our candidate from their list
```

eliminate value `v` at the `idx`th node (fix it's value, remove that value from other nodes in the same column/row/cell).

```hs
findJust [] = Nothing
findJust (Nothing:xs) = findJust xs
findJust (x:xs) = x
```

helper, get the first `Just` from a list, or `Nothing` if there is no `Just` in the list.

```hs
backtrack pos grid@(k, m) | pos == k^4 = Just grid -- we're at the end, the
                          | otherwise  = findJust [backtrack next $ (k, kill k m pos v) | v <- vs] -- try all possible values, use the first one that leads to a valid solution
                                           where (_, vs) = m !! pos -- vs are the possible values for the node at pos
                                                 next = pos + 1 -- we go from left to right, top to bottom
```

try all possible values at `pos`, and recursively try to fix the next node, until all nodes are fixed.
