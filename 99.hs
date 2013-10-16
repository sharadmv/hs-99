{- Problem 1: Find the last element of a list -}
myLast :: [a] -> a
myLast (x:xs)
        | null xs = x
        | otherwise = myLast xs

{- Problem 2: Find the second to last element of a list -}
myButLast :: [a] -> a
myButLast (x:xs)
        | length xs == 1 = x
        | otherwise = myButLast xs

{- Problem 3: Find the kth element of a list -}
elementAt :: ([a] -> Int -> a)
elementAt xs k = head . drop k $ xs

{- Problem 4: Find the number of elements in a list -}
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs)
      | null xs = 1
      | otherwise = 1 + myLength xs

{- Problem 5: Reverse a list -}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) 
      | null xs = [x]
      | otherwise = myReverse xs ++ [x]

{- Problem 6: Return if a list is a palindrome or not-}
isPalindrome :: (Eq a)=>[a] -> Bool
isPalindrome [] = True
isPalindrome (x:xs)
          | null xs = True
          | otherwise = (x == last xs) && (isPalindrome $ init xs)
