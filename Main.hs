import Data.List
import Debug.Trace

showGrid (k, m) = foldl showCell "" m
  where
    showCell = \acc -> \((col, row, cell), vs) -> acc ++ (foldl (\acc -> \i -> acc ++ if i `elem` vs then show i else "_") "" [1..k^2]) ++ " " ++ if col == (k^2 - 1) then "\n" else ""

showSol (k, m) = foldl (\acc ((col, row, cell), [x]) -> acc ++ show x ++ if col == (k^2 - 1) then "\n" else "") "" m

fromIndex i k = (col, row, cell)
  where col = i `mod` k^2
        row = i `quot` k^2
        cell = col `quot` k + (row `quot` k) * k

empty k = (k, [
  let cell = col `quot` k + (row `quot` k) * k
      values = [1..k^2]
      in ((col, row, cell), values) | row <- [0..k^2-1], col <- [0..k^2-1] ])

kill k m idx v = [update loc vs | (loc, vs) <- m] where
  (col, row, cell) = fromIndex idx k
  update loc@(col', row', cell') vs | col == col' && row == row' && cell == cell' = (loc, [v])
                                    | col /= col' && row /= row' && cell /= cell' = (loc, vs)
                                    | otherwise                                   = (loc, vs \\ [v])

-- backtrack pos grid | trace("pos: " ++ show pos ++ "\ngrid:\n" ++ showGrid grid) False = undefined
backtrack pos grid@(k, m) | pos == k^4-1 = Just grid
                          | otherwise    = case find found [backtrack next $ (k, kill k m pos v) | v <- vs ]
                                           of Nothing -> Nothing
                                              Just res -> res
                                           where (_, vs) = m !! pos
                                                 next = pos + 1
                                                 found Nothing = False
                                                 found _       = True

solve grid = backtrack 0 grid
