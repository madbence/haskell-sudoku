# haskell-sudoku

## usage

```
> putStrLn $ case solve $ empty 3 of Nothing -> "No solution"; Just sol -> "Solution:\n" ++ showSol sol
```
