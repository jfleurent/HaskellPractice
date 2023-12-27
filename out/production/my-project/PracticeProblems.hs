module PracticeProblems
  ( findLastElement,
    findSecondToLastElement,
    findKthElement,
    findNumberOfElements,
    reverseAList,
    determinePalindrome
  )
where

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 1 !!!!
-- (*) Find the last element of a list.

findLastElement :: [Int] -> IO ()
findLastElement [] = print "No such element"
findLastElement list = print ("Last element of list is : " ++ show (last list))

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 2 !!!!
-- (*) Find the last-but-one (or second-last) element of a list.

findSecondToLastElement :: [Int] -> IO ()
findSecondToLastElement [] = print "No such element"
findSecondToLastElement [_] = print "No such element"
findSecondToLastElement list = print ("Second to last element is : " ++ show (reverse list !! 1))

------------------------------------------------------------------------------------------------------------------------
-- !!!! Problem 3 !!!!
-- (*) Find the K'th element of a list.

findKthElement :: Int -> [Int] -> IO()
findKthElement index list
                        | index < length list =  print ("The element at index is : " ++ show(list !! index))
                        | otherwise = print "No such element"

 -----------------------------------------------------------------------------------------------------------------------
 -- !!!! Problem 4 !!!!
 -- (*) Find the number of elements in a list.

findNumberOfElements :: [Int] -> IO ()
findNumberOfElements list = print ("The number elements in list is : " ++ show(length list))

 -----------------------------------------------------------------------------------------------------------------------
 -- !!!! Problem 5 !!!!
-- (*) Reverse a list.

reverseAList :: [Int] -> IO ()
reverseAList list = print ("The list reversed is : " ++ show(reverse list))

 -----------------------------------------------------------------------------------------------------------------------
 -- !!!! Problem 6 !!!!
-- (*) Find out whether a list is a palindrome.

isPalindrome :: String -> Bool
isPalindrome string = and [a == b | (a,b) <- zip string (reverse string)]

determinePalindrome :: String -> IO ()
determinePalindrome string = print("String " ++ string ++ " is palindrome? : " ++ show(isPalindrome string))

------------------------------------------------------------------------------------------------------------------------