{- Exercise 1:
  Using library functions, define a function halve :: [a] -> ([a], [a]) that
  splits an even-lengthed list into two halves. For example:

  > halve [1, 2, 3, 4, 5, 6]
  ([1, 2, 3], [4, 5, 6])
-}

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
  where half = length xs `div` 2

{- Exercise 2:
  Consider a function safetail :: [a] -> [a] that behaves as the library function tail, except that safetail maps the empty list to itself, whereas tail produces an error in this case. Define safetail using:

  (a) a conditional expression;
  (b) guarded equations;
  (c) pattern matching
-}

-- (a) Using a conditional expression:
safetail_conditional :: [a] -> [a]
safetail_conditional xs = if null xs then xs else tail xs

-- (b) Using guarded equations:
safetail_guarded :: [a] -> [a]
safetail_guarded xs | null xs = []
                    | otherwise = tail xs

-- (c) Using pattern matching:
safetail_pattern :: [a] -> [a]
safetail_pattern [] = []
safetail_pattern (x:xs) = xs

{- Exercise 3:
  In a similar way to &&, show how the logical disjunction operator || can be defined in four different ways using pattern matching.

  (1)
  True || True = True
  True || False = True
  False || True = True
  False || False = False

  (2)
  False || False = False
  _ || _ = True

  (3)
  True || _ = True
  False || b = b

  (4)
  b || c  | b == c = b
          | otherwise = True
-}
