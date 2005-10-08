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
	[Disjoints (allPairs ["1","5","3","2"]) @?= Disjoints [("1","2"),("1","5"),("1","3"),("3","5"),("5","2"),("2","3")]
	
	,mmParseFromString "" @?= (ctxEmpty, Database [])
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

	,mmParseFromString "$d x $." @?= (ctxEmpty, Database [])
	,mmParseFromString "$d x y $." @?= (ctxEmpty `ctxWithDisjoints` [("y","x")], Database [])
	,mmParseFromString "$d a bb ccc $." @?= (ctxEmpty `ctxWithDisjoints` [("ccc","a"), ("a","bb"),("a","ccc")], Database [])

	,mmParseFromString "$c var $. $v x $." @?=
		(ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database []
		)

	,mmParseFromString "$c |- $. $v P $. assume-p $e |- P $." @?=
		(ctxEmpty `ctxWithConstant` "|-" `ctxWithVariable` "P"
		,Database [(True, "assume-p", [Con "|-", Var "P"], noDisjoints, DollarE)]
		)

	,mmParseFromString "$c var $. $v x $. vx $f var x $." @?=
		(ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database [(True, "vx", [Con "var", Var "x"], noDisjoints, DollarF)]
		)

	,mmParseFromString "$c term $. $v x $. tx $a term x $." @?=
		(ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "x"
		,Database [(True, "tx", [Con "term", Var "x"], noDisjoints, Axiom [])]
		)

	,findStatement (snd (mmParseFromString "$c term $. $v x $. tx $a term x $.")) "tx" @?=
		(True, "tx", [Con "term", Var "x"], noDisjoints, Axiom [])

	,mmParseFromString (unlines
		["$c term $."
		,"$v x $."
		,"ax-tx $a term x $."
		,"th-tx $p term x $= tx $."
		])
	 @?=
		(ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "x"
		,Database
			[(True, "ax-tx", [Con "term", Var "x"], noDisjoints, Axiom [])
			,(True, "th-tx", [Con "term", Var "x"], noDisjoints, Theorem [] ["tx"])
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
			[(False, "dummy", [Con "|-", Var "P"], noDisjoints, DollarF)
			,(False, "min", [Con "|-", Var "P"], noDisjoints, DollarE)
			,(False, "maj", [Con "|-", Con "(", Var "P", Con "->", Var "Q", Con ")"], noDisjoints, DollarE)
			,(True, "mp", [Con "|-", Var "Q"], noDisjoints, Axiom ["min", "maj"])
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
			[(False, "wffp", [Con "wff", Var "P"], noDisjoints, DollarF)
			,(False, "wffq", [Con "wff", Var "Q"], noDisjoints, DollarF)
			,(False, "wffr", [Con "wff", Var "R"], noDisjoints, DollarF)
			,(False, "wffs", [Con "wff", Var "S"], noDisjoints, DollarF)
			,(False, "min", [Con "|-", Var "P"], noDisjoints, DollarE)
			,(False, "maj", [Con "|-", Var "Q"], noDisjoints, DollarE)
			,(True, "mp", [Con "|-", Var "P", Var "R"], noDisjoints, Axiom ["wffp", "wffq", "wffr", "min", "maj"])
			]
		)

	,(case parse mmpCompressedNumbers "<test string>" "AAAB\nZB FAACA FAA\nFC DE $." of Left _ -> Nothing; Right l -> Just l)
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
			[(True, "tt",[Con "term",Var "t"],noDisjoints,DollarF)
			,(True, "tr",[Con "term",Var "r"],noDisjoints,DollarF)
			,(True, "ts",[Con "term",Var "s"],noDisjoints,DollarF)
			,(True, "wp",[Con "wff",Var "P"],noDisjoints,DollarF)
			,(True, "wq",[Con "wff",Var "Q"],noDisjoints,DollarF)
			,(True, "tze",[Con "term",Con "0"],noDisjoints,Axiom [])
			,(True, "tpl",[Con "term",Con "(",Var "t",Con "+",Var "r",Con ")"],noDisjoints,Axiom ["tt", "tr"])
			,(True, "weq",[Con "wff",Var "t",Con "=",Var "r"],noDisjoints,Axiom ["tt", "tr"])
			,(True, "wim",[Con "wff",Con "(",Var "P",Con "->",Var "Q",Con ")"],noDisjoints,Axiom ["wp", "wq"])
			,(True, "a1",[Con "|-",Con "(",Var "t",Con "=",Var "r",Con "->",Con "(",Var "t",Con "=",Var "s",Con "->",Var "r",Con "=",Var "s",Con ")",Con ")"],noDisjoints,Axiom ["tt", "tr", "ts"])
			,(True, "a2",[Con "|-",Con "(",Var "t",Con "+",Con "0",Con ")",Con "=",Var "t"],noDisjoints,Axiom ["tt"])
			,(False, "min",[Con "|-",Var "P"],noDisjoints,DollarE)
			,(False, "maj",[Con "|-",Con "(",Var "P",Con "->",Var "Q",Con ")"],noDisjoints,DollarE)
			,(True, "mp",[Con "|-",Var "Q"],noDisjoints,Axiom ["wp", "wq", "min", "maj"])
			,(True, "th1",[Con "|-",Var "t",Con "=",Var "t"],noDisjoints,Theorem ["tt"] ["tt","tze","tpl","tt","weq","tt","tt","weq","tt","a2","tt","tze","tpl","tt","weq","tt","tze","tpl","tt","weq","tt","tt","weq","wim","tt","a2","tt","tze","tpl","tt","tt","a1","mp","mp"])
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
		let (_, _, _, _, Theorem _ proof) = findStatement db "id"
		proof @?=
			["wph","wph","wph","wi","wi"
			,"wph","wph","wi"
			,"wph","wph","ax-1","wph"
			,"wph","wph","wi"
			,"wph","wph"
			,"wph","wph","wi"
			,"ax-1","a2i","ax-mp"
			]
	,do {(_, db) <- mmParseFromFile "peano.mm"; findStatement db "binop_plus" @?= (True, "binop_plus", [Con "BINOP", Con "+"], noDisjoints, Axiom [])}
	,do {(_, db) <- mmParseFromFile "peano.mm"; mmVerifiesDatabase db @?= True}

	,do {(_, db) <- mmParseFromFile "set-part.mm"; mmVerifiesDatabase db @?= True}

	,do {(_, db) <- mmParseFromFile "set-part2.mm"; mmVerifiesLabel db "ax17eq" @?= True}

	]

	]


ctxWithConstant :: Context -> String -> Context
ctx `ctxWithConstant` c = ctx `ctxWithConstants` [c]

ctxWithVariable :: Context -> String -> Context
ctx `ctxWithVariable` v = ctx `ctxWithVariables` [v]

noDisjoints :: Disjoints
noDisjoints = Disjoints []
