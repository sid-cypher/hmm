module Main where

import Hmm

import Test.HUnit.Base
import Test.HUnit.Text

import Text.ParserCombinators.Parsec

main :: IO ()
main = do
	 _ <- runTestTT testCases
	 return ()


testCases :: Test
testCases =
	TestList
	[

	"string-based tests" ~: test
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
		,Database [(True, "assume-p", [Con "|-", Var "P"], DollarE)]
		)

	,mmParseFromString "$c var $. $v x $. vx $f var x $." @?=
		(ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database [(True, "vx", [Con "var", Var "x"], DollarF)]
		)

	,mmParseFromString "$c term $. $v x $. tx $a term x $." @?=
		(ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "x"
		,Database [(True, "tx", [Con "term", Var "x"], Axiom [])]
		)

	,findStatement (snd (mmParseFromString "$c term $. $v x $. tx $a term x $.")) "tx" @?=
		(True, "tx", [Con "term", Var "x"], Axiom [])

	,mmParseFromString (unlines
		["$c term $."
		,"$v x $."
		,"ax-tx $a term x $."
		,"th-tx $p term x $= tx $."
		])
	 @?=
		(ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "x"
		,Database
			[(True, "ax-tx", [Con "term", Var "x"], Axiom [])
			,(True, "th-tx", [Con "term", Var "x"], Theorem [] ["tx"])
			]
		)

	,mmParseFromString "${ $}" @?= (ctxEmpty,Database [])

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
			[(False, "dummy", [Con "|-", Var "P"], DollarF)
			,(False, "min", [Con "|-", Var "P"], DollarE)
			,(False, "maj", [Con "|-", Con "(", Var "P", Con "->", Var "Q", Con ")"], DollarE)
			,(True, "mp", [Con "|-", Var "Q"], Axiom ["min", "maj"])
			]
		)

	,mmParseFromString (unlines
		["$c wff |- $."
		,"$v P Q R S $."
		,"${"
		,"	wffp $f wff P $."
		,"	wffq $f wff Q $."
		,"	wffr $f wff R $."
		,"	wffs $f wff S $."
		,"	min $e |- P $."
		,"	maj $e |- Q $."
		,"	mp  $a |- P R $."
		,"$}"
		])
	 @?=
		(ctxEmpty
		 `ctxWithConstants` ["wff", "|-"]
		 `ctxWithVariables` ["P", "Q", "R", "S"]
		,Database
			[(False, "wffp", [Con "wff", Var "P"], DollarF)
			,(False, "wffq", [Con "wff", Var "Q"], DollarF)
			,(False, "wffr", [Con "wff", Var "R"], DollarF)
			,(False, "wffs", [Con "wff", Var "S"], DollarF)
			,(False, "min", [Con "|-", Var "P"], DollarE)
			,(False, "maj", [Con "|-", Var "Q"], DollarE)
			,(True, "mp", [Con "|-", Var "P", Var "R"], Axiom ["wffp", "wffq", "wffr", "min", "maj"])
			]
		)

	--TODO: also support spaces in compressed proofs
	,(case parse mmpCompressedNumbers "<test string>" "AAABZBFAACAFAAFCDE" of Left _ -> Nothing; Right l -> Just l)
		@?= Just
		[(0,False),(0,False),(0,False),(1,True ),(1,False)
		,(5,False),(0,False),(0,False),(2,False),(0,False)
		,(5,False),(0,False),(0,False),(5,False),(2,False)
		,(3,False),(4,False)]
	],

	"file-based tests" ~: test
	[do {db <- mmParseFromFile "demo0.mm"; db @?=
		(ctxEmpty
			`ctxWithConstants` ["0","+","=","->","(",")","term","wff","|-"]
			`ctxWithVariables` ["t","r","s","P","Q"]
		,Database
			[(True, "tt",[Con "term",Var "t"],DollarF)
			,(True, "tr",[Con "term",Var "r"],DollarF)
			,(True, "ts",[Con "term",Var "s"],DollarF)
			,(True, "wp",[Con "wff",Var "P"],DollarF)
			,(True, "wq",[Con "wff",Var "Q"],DollarF)
			,(True, "tze",[Con "term",Con "0"],Axiom [])
			,(True, "tpl",[Con "term",Con "(",Var "t",Con "+",Var "r",Con ")"],Axiom ["tt", "tr"])
			,(True, "weq",[Con "wff",Var "t",Con "=",Var "r"],Axiom ["tt", "tr"])
			,(True, "wim",[Con "wff",Con "(",Var "P",Con "->",Var "Q",Con ")"],Axiom ["wp", "wq"])
			,(True, "a1",[Con "|-",Con "(",Var "t",Con "=",Var "r",Con "->",Con "(",Var "t",Con "=",Var "s",Con "->",Var "r",Con "=",Var "s",Con ")",Con ")"],Axiom ["tt", "tr", "ts"])
			,(True, "a2",[Con "|-",Con "(",Var "t",Con "+",Con "0",Con ")",Con "=",Var "t"],Axiom ["tt"])
			,(False, "min",[Con "|-",Var "P"],DollarE)
			,(False, "maj",[Con "|-",Con "(",Var "P",Con "->",Var "Q",Con ")"],DollarE)
			,(True, "mp",[Con "|-",Var "Q"],Axiom ["wp", "wq", "min", "maj"])
			,(True, "th1",[Con "|-",Var "t",Con "=",Var "t"],Theorem ["tt"] ["tt","tze","tpl","tt","weq","tt","tt","weq","tt","a2","tt","tze","tpl","tt","weq","tt","tze","tpl","tt","weq","tt","tt","weq","wim","tt","a2","tt","tze","tpl","tt","tt","a1","mp","mp"])
			]
		)}
	,do {(_, db) <- mmParseFromFile "demo0.mm"; mmComputeTheorem db ["tt"] @?= Just [Con "term", Var "t"]}
	,do {(_, db) <- mmParseFromFile "demo0.mm"; mmComputeTheorem db ["tt", "tze", "tpl"] @?=
		Just [Con "term", Con "(", Var "t", Con "+", Con "0", Con ")"]}
	,do {(_, db) <- mmParseFromFile "demo0.mm"; mmVerifiesLabel db "th1" @?= True}
	,do {(_, db) <- mmParseFromFile "demo0.mm"; mmVerifiesDatabase db @?= True}

	,do {(_, db) <- mmParseFromFile "set-part.mm"; mmVerifiesLabel db "a1i" @?= True}
	,do {(_, db) <- mmParseFromFile "set-part.mm"; mmVerifiesLabel db "a2i" @?= True}
	,do
		(_, db) <- mmParseFromFile "set-part.mm"
		let (_, _, _, Theorem _ proof) = findStatement db "id"
		proof @?=
			["wph","wph","wph","wi","wi"
			,"wph","wph","wi"
			,"wph","wph","ax-1","wph"
			,"wph","wph","wi"
			,"wph","wph"
			,"wph","wph","wi"
			,"ax-1","a2i","ax-mp"
			]
	,do {(_, db) <- mmParseFromFile "set-part.mm"; mmVerifiesDatabase db @?= True}
	]

	]


ctxWithConstant :: Context -> String -> Context
ctx `ctxWithConstant` c = ctx `ctxWithConstants` [c]

ctxWithVariable :: Context -> String -> Context
ctx `ctxWithVariable` v = ctx `ctxWithVariables` [v]
