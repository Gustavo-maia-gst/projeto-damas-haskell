module TestSimple where

import Test.HUnit

-- A simple test case for addition
testAddition :: Test
testAddition = TestCase (assertEqual "for 1 + 1," 2 (1 + 1))

-- A simple test case for subtraction (fixed the expected value)
testSubtraction :: Test
testSubtraction = TestCase (assertEqual "for 2 - 1," 1 (2 - 1))  -- Corrected the expected value
