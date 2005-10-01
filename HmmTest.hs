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
	[mmParseFromString "" @?= (ctxEmpty, Database [] [])
	,mmParseFromString " \n  " @?= (ctxEmpty, Database [] [])
	,mmParseFromString "$( $)" @?= (ctxEmpty, Database [] [])
	,mmParseFromString "$( $)         " @?= (ctxEmpty, Database [] [])
	,mmParseFromString " \t\n  $( hoi\nhoi $) " @?= (ctxEmpty, Database [] [])

	,mmParseFromString "$c x $." @?= (ctxEmpty `ctxWithConstant` "x", Database [] [])
	,mmParseFromString "$c y $." @?= (ctxEmpty `ctxWithConstant` "y", Database [] [])
	,mmParseFromString "$c xyzzy $." @?= (ctxEmpty `ctxWithConstant` "xyzzy", Database [] [])
	,mmParseFromString "$c x z y $." @?= (ctxEmpty `ctxWithConstants` ["x", "y", "z"], Database [] [])
	,mmParseFromString "$c $( no constants here $) $." @?= (ctxEmpty, Database [] [])
	,mmParseFromString "$c x $( a comment\nin the middle $) y $." @?= (ctxEmpty `ctxWithConstants` ["x", "y"], Database [] [])
	,mmParseFromString "$c x $. $c y $." @?= (ctxEmpty `ctxWithConstants` ["x", "y"], Database [] [])
	,mmParseFromString "\n$c x $.\n$c y $.\n$( final comment $)\n" @?= (ctxEmpty `ctxWithConstants` ["x", "y"], Database [] [])

	,mmParseFromString "$v x $." @?= (ctxEmpty `ctxWithVariable` "x", Database [] [])
	,mmParseFromString "$v y $." @?= (ctxEmpty `ctxWithVariable` "y", Database [] [])
	,mmParseFromString "$v xyzzy $." @?= (ctxEmpty `ctxWithVariable` "xyzzy", Database [] [])
	,mmParseFromString "$v x z y $." @?= (ctxEmpty `ctxWithVariables` ["x", "y", "z"], Database [] [])
	,mmParseFromString "$v $( no constants here $) $." @?= (ctxEmpty, Database [] [])
	,mmParseFromString "$v x $( a comment\nin the middle $) y $." @?= (ctxEmpty `ctxWithVariables` ["x", "y"], Database [] [])
	,mmParseFromString "$v x $. $v y $." @?= (ctxEmpty `ctxWithVariables` ["x", "y"], Database [] [])
	,mmParseFromString "\n$v x $.\n$v y $.\n$( final comment $)\n" @?= (ctxEmpty `ctxWithVariables` ["x", "y"], Database [] [])

	,mmParseFromString "$c var $. $v x $." @?=
		(ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database [] []
		)

	,mmParseFromString "$c |- $. $v P $. assume-p $e |- P $." @?=
		(ctxEmpty `ctxWithConstant` "|-" `ctxWithVariable` "P"
		,Database [] [("assume-p", [Con "|-", Var "P"], DollarE)]
		)

	,mmParseFromString "$c var $. $v x $. vx $f var x $." @?=
		(ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database [] [("vx", [Con "var", Var "x"], DollarF)]
		)

	,mmParseFromString "$c term $. $v x $. tx $a term x $." @?=
		(ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "x"
		,Database [] [("tx", [Con "term", Var "x"], Axiom)]
		)

	,mmParseFromString (unlines
		["$c term $."
		,"$v x $."
		,"ax-tx $a term x $."
		,"th-tx $p term x $= tx $."
		])
	 @?=
		(ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "x"
		,Database
			[
			]
			[("ax-tx", [Con "term", Var "x"], Axiom)
			,("th-tx", [Con "term", Var "x"], Theorem ["tx"])
			]
		)

	,mmParseFromString "${ $}" @?= (ctxEmpty,Database [] [])

	,mmParseFromString (unlines
		["$c |- ( -> ) $."
		,"$v P Q $."
		,"${"
		,"	${ dummy $f |- P $. $}"
		,"	min $e |- P $."
		,"	maj $e |- ( P -> Q ) $."
		,"	mp  $a |- Q $."
		,"$}"
		])
	 @?=
		(ctxEmpty
		 `ctxWithConstants` ["|-", "(", ")", "->"]
		 `ctxWithVariables` ["P", "Q"]
		,Database
			[("dummy", [Con "|-", Var "P"], DollarF)
			,("min", [Con "|-", Var "P"], DollarE)
			,("maj", [Con "|-", Con "(", Var "P", Con "->", Var "Q", Con ")"], DollarE)
			]
			[("mp", [Con "|-", Var "Q"], Axiom)
			]
		)
	]

ctx `ctxWithConstant` c = ctx `ctxWithConstants` [c]
ctx `ctxWithVariable` v = ctx `ctxWithVariables` [v]
