{- Exercise 1:
  Using library functions, define a function halve :: [a] -> ([a], [a]) that
  splits an even-lengthed list into two halves. For example:

  > halve [1, 2, 3, 4, 5, 6]
  ([1, 2, 3], [4, 5, 6])
-}

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
  where half = length xs `div` 2
