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
  In a similar way to &&, show how the logical disjunction operator || can be
  defined in four different ways using pattern matching.

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

{- Exercise 4:
  Redefine the following version of the conjuntion operator using conditional
  expressions rather than pattern matching:

  True && b = b
  False && _ = False

  a || b = if a then b else False
-}

{- Exercise 5:
  Do the same for the following version, and note the difference in the number of conditional expressions required:

  True && True = True
  _ && _ = False

  a || b = if a then
             if b then True else False
           else
             False
-}

{- Exercise 6:
  Show how the curried function definition mult x y z = x * y * z can be
  understood in terms of lambda expressions.

  mult = \x => (\y => (\z => x * y * z))
-}
