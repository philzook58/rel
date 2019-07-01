
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}


import Test.QuickCheck
import Test.QuickCheck.All
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
import RelTest
{-
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

prop_commutativeAdd :: Int -> Int -> Bool
prop_commutativeAdd x y = x + y == y + x
-}

-- main :: IO Bool
-- return []
main = mycheck -- $quickCheckAll -- checkParallel $$(discover) -- putStrLn "Test suite not yet implemented"

{-
do 
                            a <- Gen.alpha 
                            i <- Gen.int (Range.linear 0 100)
-}