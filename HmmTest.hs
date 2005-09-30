module Main where

import Hmm

import Test.HUnit.Base
import Test.HUnit.Text

main :: IO ()
main = do
	 _ <- runTestTT testCases
	 return ()


testCases :: Test
testCases =
	test
	[mmLabels (mmParseFromString "") @=? []
	,mmLabels (mmParseFromString " \n  ") @=? []
	,mmLabels (mmParseFromString "$( $)") @=? []
	,mmLabels (mmParseFromString "$( $)         ") @=? []
	,mmLabels (mmParseFromString " \t\n  $( hoi\nhoi $) ") @=? []
	]
