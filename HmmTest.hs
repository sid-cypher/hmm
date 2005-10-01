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
	[mmParseFromString "" @=? (ctxEmpty, Database)
	,mmParseFromString " \n  " @=? (ctxEmpty, Database)
	,mmParseFromString "$( $)" @=? (ctxEmpty, Database)
	,mmParseFromString "$( $)         " @=? (ctxEmpty, Database)
	,mmParseFromString " \t\n  $( hoi\nhoi $) " @=? (ctxEmpty, Database)
	,mmParseFromString "$c x $." @=? (ctxEmpty `ctxWithConstant` "x", Database)
	,mmParseFromString "$c y $." @=? (ctxEmpty `ctxWithConstant` "y", Database)
	,mmParseFromString "$c x z y $." @=? (ctxEmpty `ctxWithConstants` ["x", "y", "z"], Database)
	,mmParseFromString "$c $( no constants here $) $." @=? (ctxEmpty, Database)
	,mmParseFromString "$c x $( a comment\nin the middle $) y $." @=? (ctxEmpty `ctxWithConstants` ["x", "y"], Database)
	]

ctx `ctxWithConstant` c = ctx `ctxWithConstants` [c]
