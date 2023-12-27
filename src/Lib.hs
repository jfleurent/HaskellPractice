module Lib
  ( printCylinderArea,
    printBoxArea,
    printListWithStartAndEnd,
    printPow4List,
    nameProgram,
    printFibNumber,
    printFibList,
    printIndexOfFibList
  )
where

------------------------------------------------------------------------------------------------------------------------
-- Utils
sqConstant :: Int
sqConstant = 2

sq :: Float -> Float
sq a = a ^ sqConstant

circleArea :: Float -> Float
circleArea r = pi * sq r

------------------------------------------------------------------------------------------------------------------------
-- !!!! Initial Practice !!!!

--sqConstant :: Int
--sqConstant = 2
--
--sq :: Floating a => a -> a
--sq a = a ^ sqConstant
--
--circleArea :: Floating a => a -> a
--circleArea x = pi * sq x
--
--value :: Float
--value = 2
--someFunc = print (sq (sq (sq value)))

------------------------------------------------------------------------------------------------------------------------
-- !!!! Exercise 1 Find Area of a Box using functions !!!!

boxArea :: Int -> Int -> Int -> Int
boxArea h w l = h * w * l

printBoxArea :: Int -> Int -> Int -> IO ()
printBoxArea h w l = print ("Area of a box is: " ++ show (boxArea h w l))

------------------------------------------------------------------------------------------------------------------------
-- !!!! Exercise 2 Find Area of a Cylinder using functions !!!!

cylinderArea :: Float -> Float -> Float
cylinderArea h r = circleArea r * h

printCylinderArea :: Float -> Float -> IO ()
printCylinderArea h r = print ("Area of Cylinder is: " ++ show (cylinderArea h r))

------------------------------------------------------------------------------------------------------------------------
-- !!!! List Practice !!!!

list :: Int -> Int -> [Int]
list start end = [start .. end]

printListWithStartAndEnd :: Int -> Int -> IO ()
printListWithStartAndEnd start end = print (list start end)

powOf4Constant :: Int
powOf4Constant = 4

pow4List :: Int -> Int -> [Int]
pow4List start end = [n ^ powOf4Constant | n <- list start end]

printPow4List :: Int -> Int -> IO ()
printPow4List start end = print (pow4List start end)

------------------------------------------------------------------------------------------------------------------------
-- !!!! Command Line Test !!!!

namePrompt :: IO ()
namePrompt = putStrLn "What your name?"

displayName :: String -> IO ()
displayName name = putStrLn ("Your name is: " ++ name)

-- Doesn't work as expected in IDE run :/
nameProgram :: IO ()
nameProgram = do
  namePrompt
  name <- getLine
  displayName name

------------------------------------------------------------------------------------------------------------------------
-- !!!! Recursion : Fibonacci Sequence !!!!

-- Can't go to high values due to call stack limitation
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x -1) + fib (x -2)

printFibNumber :: Int -> IO ()
printFibNumber x = print (fib x)

-- Uses memoization to calculate fib numbers
fibList :: [Integer]
fibList = 0 : 1 : 1 : [ a + b | (a, b) <- zip fibList (tail fibList)]

printFibList ::Int -> IO()
printFibList end = print $ take end fibList

printIndexOfFibList :: Int -> IO()
printIndexOfFibList index = print $ fibList !! index
