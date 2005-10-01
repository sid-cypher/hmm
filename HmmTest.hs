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
	[mmParseFromString "" @=? dbEmpty
	,mmParseFromString " \n  " @=? dbEmpty
	,mmParseFromString "$( $)" @=? dbEmpty
	,mmParseFromString "$( $)         " @=? dbEmpty
	,mmParseFromString " \t\n  $( hoi\nhoi $) " @=? dbEmpty
	,mmParseFromString "$c x $." @=? dbEmpty `dbWithConstant` "x"
	,mmParseFromString "$c y $." @=? dbEmpty `dbWithConstant` "y"
	,mmParseFromString "$c x z y $." @=? dbEmpty `dbWithConstants` ["x", "y", "z"]
	,mmParseFromString "$c $( no constants here $) $." @=? dbEmpty
	,mmParseFromString "$c x $( a comment\nin the middle $) y $." @=? dbEmpty `dbWithConstants` ["x", "y"]
	]

db `dbWithConstant` c = db `dbWithConstants` [c]
