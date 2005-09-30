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
	[mmParseFromString "" @=? emptyDatabase
	,mmParseFromString " \n  " @=? emptyDatabase
	,mmParseFromString "$( $)" @=? emptyDatabase
	,mmParseFromString "$( $)         " @=? emptyDatabase
	,mmParseFromString " \t\n  $( hoi\nhoi $) " @=? emptyDatabase
	,mmParseFromString "$c x $." @=? emptyDatabase `withConstant` "x"
	,mmParseFromString "$c y $." @=? emptyDatabase `withConstant` "y"
	,mmParseFromString "$c x z y $." @=? emptyDatabase `withConstants` ["x", "y", "z"]
	,mmParseFromString "$c $( no constants here $) $." @=? emptyDatabase
	,mmParseFromString "$c x $( a comment\nin the middle $) y $." @=? emptyDatabase `withConstants` ["x", "y"]
	]

emptyDatabase = Database {dbConstants = []}

(Database cs) `withConstant` c = Database {dbConstants = c:cs}
(Database cs) `withConstants` cs2 = Database {dbConstants = cs2++cs}
