{-# LANGUAGE OverloadedStrings #-}
import           Lib
import           Test.HUnit

halveEvensTests :: Test
halveEvensTests = TestList
  [
    TestCase $ assertEqual "Empty list" [] $ halveEvens []
  , TestCase $ assertEqual "[1,2,3,4,5]" [1,2] $ halveEvens [1,2,3,4,5]
  , TestCase $ assertEqual "[6,6,6,3,3,3,2,2,2]"
               [3,3,3,1,1,1] $ halveEvens [6,6,6,3,3,3,2,2,2]
  ]

safeStringTests :: Test
safeStringTests = TestList
  [
    TestCase $ assertEqual "Hello World!"
               "Hello World!" $ safeString "Hello World!"
  , TestCase $ assertEqual "That’s your line:\n"
               "That_s your line:_" $ safeString "That’s your line:\n"
  , TestCase $ assertEqual "🙋.o(“Me Me Me”)"
               "_.o(_Me Me Me_)" $ safeString "🙋.o(“Me Me Me”)"
  ]

holesTests :: Test
holesTests = TestList
  [
    TestCase $ assertEqual "[1,2,3]" [[2,3], [1,3], [1,2]] $ holes [1,2,3]
  , TestCase $ assertEqual
               "Hello" ["ello", "Hllo", "Helo", "Helo", "Hell"] $ holes "Hello"
  ]

longestTextTests :: Test
longestTextTests = TestList
  [
    TestCase $ assertEqual "[True,False" False $ longestText [True,False]
  , TestCase $ assertEqual "[2,4,16,32]" 32 $ longestText [2,4,16,32]
  , TestCase $ assertEqual "(words \"Hello World\")"
               "World" $ longestText $ words "Hello World"
  , TestCase $ assertEqual "[True,False]"
               "Olá" $ longestText $ words "Olá mundo"
  ]

adjacentsTests :: Test
adjacentsTests = TestList
  [
    TestCase $ assertEqual "Empty string" [] $ adjacents ""
  , TestCase $ assertEqual "[True]" [] $ adjacents [True]
  , TestCase $ assertEqual "Hello"
               [('H','e'),('e','l'),('l','l'),('l','o')] $ adjacents "Hello"
  ]

commasTests :: Test
commasTests = TestList
  [
    TestCase $ assertEqual "Empty string" "" $ commas []
  , TestCase $ assertEqual "Hello" "Hello" $ commas ["Hello"]
  , TestCase $ assertEqual "[\"Hello\", \"World\"]"
               "Hello, World" $ commas ["Hello", "World"]
  , TestCase $ assertEqual "[\"Hello\", \"\", \"World\"]"
               "Hello, , World" $ commas ["Hello", "", "World"]
  , TestCase $ assertEqual "[\"Hello\", \"new\", \"World\"]"
               "Hello, new, World" $ commas ["Hello", "new", "World"]
  ]

addPolynomialsTests :: Test
addPolynomialsTests = TestList
  [
    TestCase $ assertEqual "Empty list" [] $ addPolynomials [[]]
  , TestCase $ assertEqual "[[0, 1], [1, 1]]"
               [1,2] $ addPolynomials [[0, 1], [1, 1]]
  , TestCase $ assertEqual "[[0, 1, 5], [7, 0, 0], [-2, -1, 5]]"
               [5,0,10] $ addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]]
  ]

sumNumbersTests :: Test
sumNumbersTests = TestList
  [
    TestCase $ assertEqual "Empty string" 0 $ sumNumbers ""
  , TestCase $ assertEqual "Hello world!" 0 $ sumNumbers "Hello world!"
  , TestCase $ assertEqual "a1bc222d3f44" 270 $ sumNumbers "a1bc222d3f44"
  , TestCase $ assertEqual
               "words0are1234separated12by3integers45678" 46927 $
                 sumNumbers "words0are1234separated12by3integers45678"
  , TestCase $ assertEqual "000a." 0 $ sumNumbers "000a."
  , TestCase $ assertEqual "0.00a." 0 $ sumNumbers "0.00a."
  ]

wordCountTests :: Test
wordCountTests = TestList
  [
    TestCase $ assertEqual "World count"
               ("Number of lines: 7" ++
               "\nNumber of empty lines: 6" ++
               "\nNumber of words: 59" ++
               "\nNumber of unique words: 48" ++
               "\nNumber of words followed by themselves:1" ++
               "\nLength of the longest line: 70") $
               wordCount $
               "This exercise is mostly about discovering the functions " ++
               "\n" ++
               "\nprovided by the Prelude Prelude and other modules such as Data.List, " ++
               "\nData.Char, Data.Ord, Data.Function. Try to implement the following " ++
               "\nfunctions while making best use of the provided library functions. " ++
               "\nNone of these should require you to write a recursive function! " ++
               "\nUse the given examples to understand the function better, if required."
  ]

fibTests :: Test
fibTests = TestList
  [
    TestCase $ assertEqual "fib 15 == 610" 610 $ fib 14
  , TestCase $ assertEqual "fibs1 == [1,1,2,3,5,..]" [1,1,2,3,5] $ take 5 fibs1
  ]

supplyTests :: Test
supplyTests = TestList
  [
    TestCase $ assertEqual "42 == 42" 42 $ runSupply nats $ pure 42
  ]

tests :: Test
tests = TestList
  [
    halveEvensTests
  , safeStringTests
  , holesTests
  , longestTextTests
  , adjacentsTests
  , commasTests
  , addPolynomialsTests
  , sumNumbersTests
  , wordCountTests
  , fibTests
  , supplyTests
  ]

main :: IO Counts
main = runTestTT tests
