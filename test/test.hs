import ClassyPrelude
import Parser (identifier, lexeme)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Megaparsec (parseMaybe)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, exampleTestCase]

exampleTestCase =
  testCase "Example test case" $ do
    -- assertion no. 1 (passes)
    2 + 2 @?= 4
    -- assertion no. 2 (fails)
    assertBool "the list is not empty" $ null [1]
    -- assertion no. 3 (would have failed, but won't be executed because
    -- the previous assertion has already failed)
    "foo" @?= "bar"

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Unit tests"
    [ testCase "identifier +" $ parseMaybe identifier "+" @?= Just "+",
      testCase "identifier does not consume trailing space" $
        parseMaybe identifier "+ " @?= Nothing,
      testCase "lexeme identifier consumes trailing space" $
        parseMaybe (lexeme identifier) "+ " @?= Just "+"
    ]
