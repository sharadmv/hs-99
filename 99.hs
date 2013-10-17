{- Problem 1: Find the last element of a list -}
myLast :: [a] -> a
myLast [] = error "Can't find last element of empty list"
myLast (x:xs)
        | null xs = x
        | otherwise = myLast xs

{- Problem 2: Find the second to last element of a list -}
myButLast :: [a] -> a
myButLast [] = error "Can't find second to last element of empty list"
myButLast (x:xs)
        | length xs == 1 = error "Can't find second to last element of one list"
        | length xs == 2 = x
        | otherwise = myButLast xs

{- Problem 3: Find the kth element of a list -}
elementAt :: ([a] -> Int -> a)
elementAt xs k = head . drop (k - 1) $ xs

{- Problem 4: Find the number of elements in a list -}
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs)
      | null xs = 1
      | otherwise = 1 + myLength xs

{- Problem 5: Reverse a list -}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) 
      | null xs = [x]
      | otherwise = myReverse xs ++ [x]

{- Problem 6: Return if a list is a palindrome or not-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:xs)
          | null xs = True
          | otherwise = (x == last xs) && (isPalindrome $ init xs)

{- Problem 7: Flatten a nested list structure -}
data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List a) = foldr (\x y -> flatten x++y) [] a

{- Problem 8: Remove consecutive duplicates of list elements -}
compress :: (Eq a) => [a] -> [a]
compress = foldl (\x y -> if null x then [y] else if last x == y then x else x++[y]) []

{- Problem 9: Pack consecutive duplicates of list elements into sublists -}
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = let (h, t) = (span (==(head xs)) xs) in [h] ++ pack t 

{- Problem 10:  Run-length encoding of a list -}
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack



{- Main method. Running examples -}
main :: IO()
main = do
          putStrLn "Running examples..."
          putStrLn . show $ myLast [1,2,3,4] -- 4
          putStrLn . show $ myLast ['x','y','z'] -- 'z'
          putStrLn . show $ myButLast [1,2,3,4] -- 3
          putStrLn . show $ myButLast ['a'..'z'] -- 'y'
          putStrLn . show $ elementAt [1,2,3] 2
          putStrLn . show $ elementAt "haskell" 5
          putStrLn . show $ myLength [123, 456, 789]
          putStrLn . show $ myLength "Hello, world!"
          putStrLn . show $ myReverse "A man, a plan, a canal, panama!"
          putStrLn . show $ myReverse [1,2,3,4]
          putStrLn . show $ myLength [123, 456, 789]
          putStrLn . show $ isPalindrome [1,2,3]
          putStrLn . show $ isPalindrome "madamimadam"
          putStrLn . show $ isPalindrome [1,2,4,8,16,8,4,2,1]
          putStrLn . show $ flatten (Elem 5)
          putStrLn . show $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
          putStrLn . show $ (flatten (List []) :: [Int])
          putStrLn . show $ compress "aaaabccaadeeee"
          putStrLn . show $  ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
          putStrLn . show $  encode "aaaabccaadeeee"

