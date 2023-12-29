module PracticeProblems
  ( findLastElement,
    findSecondToLastElement,
    findKthElement,
    findNumberOfElements,
    reverseAList,
    determinePalindrome,
    flatten,
    compress,
    pack,
    encode,
    encodeToString,
    decodeStringList
  )
where

import Data.Char (ord)

--All problems are found here: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 1 !!!!
-- (*) Find the last element of a list.

findLastElement :: [Int] -> String
findLastElement [] = "No such element"
findLastElement list = "Last element of list is : " ++ show (last list)

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 2 !!!!
-- (*) Find the last-but-one (or second-last) element of a list.

findSecondToLastElement :: [Int] -> String
findSecondToLastElement [] = "No such element"
findSecondToLastElement [_] = "No such element"
findSecondToLastElement (_ : x : _) = "Second to last element is : " ++ show x

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 3 !!!!
-- (*) Find the K'th element of a list.

findKthElement :: Int -> [Int] -> String
findKthElement index list
  | index < length list = "The element at index is : " ++ show (list !! index)
  | otherwise = "No such element"

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 4 !!!!
-- (*) Find the number of elements in a list.

findNumberOfElements :: [Int] -> String
findNumberOfElements list = "The number elements in list is : " ++ show (length list)

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 5 !!!!
-- (*) Reverse a list.

reverseAList :: [Int] -> String
reverseAList list = "The list reversed is : " ++ show (reverse list)

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 6 !!!!
-- (*) Find out whether a list is a palindrome.

isPalindrome :: String -> Bool
isPalindrome string = and [a == b | (a, b) <- zip string (reverse string)]

determinePalindrome :: String -> String
determinePalindrome string = "String " ++ string ++ " is palindrome? : " ++ show (isPalindrome string)

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 7 !!!!
-- (**) Flatten a nested list structure.

-- TODO Figure out how this structure works
--data NestedList a = Elem a | List[NestedList a]
--
--list :: NestedList Integer
--list = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]

flatten :: [[Integer]] -> [Integer]
flatten = concat

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 8 !!!!
-- (**) Eliminate consecutive duplicates of list elements.

compress :: String -> String
compress [] = []
compress [x] = [x]
compress (x1 : x2 : xs)
  | x1 == x2 = compress (x2 : xs)
  | otherwise = x1 : compress (x2 : xs)

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 9 !!!!
-- (**) Pack consecutive duplicates of list elements into sublists.

pack :: String -> [String]
pack [] = []
pack (x : xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 10 !!!!
-- (*) Run-length encoding of a list.

encode :: String -> [(Int, Char)]
encode s = zip (map length l) (map head l)
  where
    l = pack s

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 11 !!!!
-- (*) Modified run-length encoding.

encodedMessage :: (Int, Char) -> String
encodedMessage m
  | fst m > 1 = "Multiple " ++ show (fst m) ++ " " ++ show (snd m)
  | fst m == 1 = "Single " ++ show (snd m)
  | otherwise = ""

encodeToString :: String -> [String]
encodeToString s = map encodedMessage $ encode s

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 12 !!!!
-- (**) Decode a run-length encoded list.

generateString :: Char -> Int -> String
generateString _ 0 = ""
generateString c x = c : generateString c (x - 1)

decodeString :: String -> String
decodeString [] = ""
decodeString (s : ss)
  | s `elem` "M" = generateString (ss !! 11)  ((ord (ss !! 8)) - 48) -- Might have to read number for multiple digits
  | s `elem` "S" = generateString (ss !! 7) 1
  | otherwise = ""

decodeStringList :: [String] -> String
decodeStringList s = concat $ map decodeString s
