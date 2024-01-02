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
    decodeStringList,
    encodeDirect,
    dupli,
    repli,
    dropEvery,
    split',
    slice,
    rotate,
    removeAt,
    insertAt,
    range,
    rnd_select,
    diff_select,
  )
where

import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty, fromList, sortBy, toList)
import System.Random

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
  | s `elem` "M" = generateString (ss !! 11) ((ord (ss !! 8)) - 48) -- Might have to read number for multiple digits
  | s `elem` "S" = generateString (ss !! 7) 1
  | otherwise = ""

decodeStringList :: [String] -> String
decodeStringList s = concat $ map decodeString s

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 13 !!!!
-- (**) Run-length encoding of a list (direct solution)

countChar :: String -> [(Int, Char)]
countChar [] = []
countChar (x : xs) = (length ((x : takeWhile (== x) xs)), x) : countChar (dropWhile (== x) xs)

encodeDirect :: String -> [String]
encodeDirect s = map encodedMessage (countChar s)

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 14 !!!!
-- (*) Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = replicate 2 x ++ dupli xs

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 15 !!!!
-- (**) Replicate the elements of a list a given number of times.

repli :: String -> Int -> String
repli [] _ = []
repli _ 0 = []
repli (x : xs) y = replicate y x ++ repli xs y

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 16 !!!!
-- (**) Drop every N'th element from a list.

dropEvery :: String -> Int -> String
dropEvery [] _ = []
dropEvery x 0 = x
dropEvery x y = (take (y -1) x) ++ dropEvery (drop y x) y

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 17 !!!!
-- (*) split' a list into two parts; the length of the first part is given.

collect :: String -> Int -> String
collect [] _ = []
collect _ 0 = []
collect (x : xs) y = x : collect xs (y - 1)

goto :: String -> Int -> String
goto [] _ = []
goto x 0 = x
goto (x : xs) y
  | y > 0 = goto xs (y - 1)
  | y == 0 = xs
  | (x : xs) == [] = []
  | otherwise = []

split' :: String -> Int -> [String]
split' x y = collect x y : [goto x y]

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 18 !!!!
-- (**) Extract a slice from a list.

slice :: String -> Int -> Int -> String
slice x y z = drop (y -1) $ take z x

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 19 !!!!
-- (**) Rotate a list N places to the left.

rotate :: String -> Int -> String
rotate [] _ = []
rotate x 0 = x
rotate x y
  | y > 0 = drop index x ++ take index x
  | y < 0 = drop reverseIndex x ++ take reverseIndex x
  where
    index = (y `mod` length x); reverseIndex = abs $ (abs y `mod` length x) - length x

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 20 !!!!
-- (*) Remove the K'th element from a list.

removeAt :: Int -> String -> Maybe (Char, String)
removeAt _ [] = Nothing
removeAt 0 _ = Nothing
removeAt x y
  | start >= 0 && end < length y = Just (slice y x end !! 0, take start y ++ drop x y)
  where
    start = x -1; end = x + 1

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 21 !!!!
-- Insert an element at a given position into a list.

insertAt :: Char -> String -> Int -> String
insertAt x [] _ = [x]
insertAt x y 0 = x : y
insertAt x y z = (string !! 0) ++ x : string !! 1
  where
    string = split' y (z - 1)

-----------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 22 !!!!
-- Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range x y
  | x <= y = x : range (x + 1) y
  | x > y = []

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 23 !!!!
-- Extract a given number of randomly selected elements from a list.

randomNumbers :: Int -> String -> [Int]
randomNumbers x y = randomRs (0, ((length y) - 1)) (mkStdGen x)

pairedList :: [Int] -> String -> NonEmpty (Int, Char)
pairedList x y = fromList $ zip x y

comparePairs :: (Int, Char) -> (Int, Char) -> Ordering
comparePairs (r1, _) (r2, _) = compare r1 r2

sortedPairs :: NonEmpty (Int, Char) -> NonEmpty (Int, Char)
sortedPairs x = sortBy comparePairs x

selectedElements :: [(Int, Char)] -> String
selectedElements x = map snd x

genSeedFromString :: String -> IO Int
genSeedFromString y = randomRIO (0, ((length y) - 1))

rnd_select :: Int -> String -> IO ()
rnd_select 0 _ = print ""
rnd_select x y = do
  seed <- genSeedFromString y
  let r = randomNumbers seed y; p = pairedList r y; s = sortedPairs p; s2 = selectedElements $ toList s
  print $ take x s2

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 24 !!!!
-- Lotto: Draw N different random numbers from the set 1..M.

diff_select :: Int -> Int -> IO ()
diff_select x y = do
  z <- randomRIO (0, y)
  print $ take x $ randomRs (1, y) (mkStdGen z)
