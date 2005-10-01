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
	[mmParseFromString "" @?= (ctxEmpty, Database [])
	,mmParseFromString " \n  " @?= (ctxEmpty, Database [])
	,mmParseFromString "$( $)" @?= (ctxEmpty, Database [])
	,mmParseFromString "$( $)         " @?= (ctxEmpty, Database [])
	,mmParseFromString " \t\n  $( hoi\nhoi $) " @?= (ctxEmpty, Database [])

	,mmParseFromString "$c x $." @?= (ctxEmpty `ctxWithConstant` "x", Database [])
	,mmParseFromString "$c y $." @?= (ctxEmpty `ctxWithConstant` "y", Database [])
	,mmParseFromString "$c xyzzy $." @?= (ctxEmpty `ctxWithConstant` "xyzzy", Database [])
	,mmParseFromString "$c x z y $." @?= (ctxEmpty `ctxWithConstants` ["x", "y", "z"], Database [])
	,mmParseFromString "$c $( no constants here $) $." @?= (ctxEmpty, Database [])
	,mmParseFromString "$c x $( a comment\nin the middle $) y $." @?= (ctxEmpty `ctxWithConstants` ["x", "y"], Database [])
	,mmParseFromString "$c x $. $c y $." @?= (ctxEmpty `ctxWithConstants` ["x", "y"], Database [])
	,mmParseFromString "\n$c x $.\n$c y $.\n$( final comment $)\n" @?= (ctxEmpty `ctxWithConstants` ["x", "y"], Database [])

	,mmParseFromString "$v x $." @?= (ctxEmpty `ctxWithVariable` "x", Database [])
	,mmParseFromString "$v y $." @?= (ctxEmpty `ctxWithVariable` "y", Database [])
	,mmParseFromString "$v xyzzy $." @?= (ctxEmpty `ctxWithVariable` "xyzzy", Database [])
	,mmParseFromString "$v x z y $." @?= (ctxEmpty `ctxWithVariables` ["x", "y", "z"], Database [])
	,mmParseFromString "$v $( no constants here $) $." @?= (ctxEmpty, Database [])
	,mmParseFromString "$v x $( a comment\nin the middle $) y $." @?= (ctxEmpty `ctxWithVariables` ["x", "y"], Database [])
	,mmParseFromString "$v x $. $v y $." @?= (ctxEmpty `ctxWithVariables` ["x", "y"], Database [])
	,mmParseFromString "\n$v x $.\n$v y $.\n$( final comment $)\n" @?= (ctxEmpty `ctxWithVariables` ["x", "y"], Database [])

	,mmParseFromString "$c var $. $v x $." @?=
		(ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database []
		)

	,mmParseFromString "$c |- $. $v P $. assume-p $e |- P $." @?=
		(ctxEmpty `ctxWithConstant` "|-" `ctxWithVariable` "P"
		,Database [("assume-p", [Con "|-", Var "P"], DollarE)]
		)

	,mmParseFromString "$c var $. $v x $. vx $f var x $." @?=
		(ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database [("vx", [Con "var", Var "x"], DollarF)]
		)

	,mmParseFromString "$c term $. $v 0 $. tze $a term 0 $." @?=
		(ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "0"
		,Database [("tze", [Con "term", Var "0"], Axiom)]
		)

	,mmParseFromString "$c term $. $v 0 $. ax-tze $a term 0 $. th-tze $p term 0 $= tze $." @?=
		(ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "0"
		,Database
			[("ax-tze", [Con "term", Var "0"], Axiom)
			,("th-tze", [Con "term", Var "0"], Theorem ["tze"])
			]
		)
	]

ctx `ctxWithConstant` c = ctx `ctxWithConstants` [c]
ctx `ctxWithVariable` v = ctx `ctxWithVariables` [v]
