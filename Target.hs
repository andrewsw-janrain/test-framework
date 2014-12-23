module Target where

import Test.HUnit

test_1 :: IO ()
test_1 = singleTest $ TestCase $ assertEqual "foo" 2 (1+1) -- putStrLn "test 1"

test_2 :: IO ()
test_2 = singleTest $ TestCase $ assertBool "Failing bool test" False

testThree :: IO ()
testThree = singleTest $ TestLabel "Here is a labeled group of tests"
            $ TestList [ TestCase $ assertEqual "it works" 1 1
                       , TestCase$ assertEqual "really" 1 1
                       ]


another_func :: IO ()
another_func = putStrLn "another func" >> putStrLn "yep"


singleTest :: Test -> IO ()
singleTest t = runTestTT t >>= \_ -> return ()