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
	
	,mmParseFromString "" @?= Right (ctxEmpty, Database [])
	,mmParseFromString " \n  " @?= Right (ctxEmpty, Database [])
	,mmParseFromString "$( $)" @?= Right (ctxEmpty, Database [])
	,mmParseFromString "$( $)         " @?= Right (ctxEmpty, Database [])
	,mmParseFromString " \t\n  $( hoi\nhoi $) " @?= Right (ctxEmpty, Database [])

	,mmParseFromString "$c x $." @?= Right (ctxEmpty `ctxWithConstant` "x", Database [])
	,mmParseFromString "$c y $." @?= Right (ctxEmpty `ctxWithConstant` "y", Database [])
	,mmParseFromString "$c xyzzy $." @?= Right (ctxEmpty `ctxWithConstant` "xyzzy", Database [])
	,mmParseFromString "$c x z y $." @?= Right (ctxEmpty `ctxWithConstants` ["x", "y", "z"], Database [])
	,mmParseFromString "$c $( no constants here $) $." @?= Right (ctxEmpty, Database [])
	,mmParseFromString "$c x $( a comment\nin the middle $) y $." @?= Right (ctxEmpty `ctxWithConstants` ["x", "y"], Database [])
	,mmParseFromString "$c x $. $c y $." @?= Right (ctxEmpty `ctxWithConstants` ["x", "y"], Database [])
	,mmParseFromString "\n$c x $.\n$c y $.\n$( final comment $)\n" @?= Right (ctxEmpty `ctxWithConstants` ["x", "y"], Database [])

	,mmParseFromString "$v x $." @?= Right (ctxEmpty `ctxWithVariable` "x", Database [])
	,mmParseFromString "$v y $." @?= Right (ctxEmpty `ctxWithVariable` "y", Database [])
	,mmParseFromString "$v xyzzy $." @?= Right (ctxEmpty `ctxWithVariable` "xyzzy", Database [])
	,mmParseFromString "$v x z y $." @?= Right (ctxEmpty `ctxWithVariables` ["x", "y", "z"], Database [])
	,mmParseFromString "$v $( no constants here $) $." @?= Right (ctxEmpty, Database [])
	,mmParseFromString "$v x $( a comment\nin the middle $) y $." @?= Right (ctxEmpty `ctxWithVariables` ["x", "y"], Database [])
	,mmParseFromString "$v x $. $v y $." @?= Right (ctxEmpty `ctxWithVariables` ["x", "y"], Database [])
	,mmParseFromString "\n$v x $.\n$v y $.\n$( final comment $)\n" @?= Right (ctxEmpty `ctxWithVariables` ["x", "y"], Database [])

	,mmParseFromString "$d x $." @?= Right (ctxEmpty, Database [])
	,mmParseFromString "$d x y $." @?= Right (ctxEmpty `ctxWithDisjoints` [("y","x")], Database [])
	,mmParseFromString "$d a bb ccc $." @?= Right (ctxEmpty `ctxWithDisjoints` [("ccc","a"), ("a","bb"),("bb","ccc")], Database [])
	,mmParseFromString "$d x y $. $d x z $." @?= Right (ctxEmpty `ctxWithDisjoints` [("y","x"), ("x","z")], Database [])
	,mmParseFromString "${ $d x y $. $} $d x z $." @?= Right (ctxEmpty `ctxWithDisjoints` [("x","z")], Database [])

	,mmParseFromString "$c var $. $v x $." @?=
		Right (ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database []
		)

	,mmParseFromString "$c |- $. $v P $. assume-p $e |- P $." @?=
		Right (ctxEmpty `ctxWithConstant` "|-" `ctxWithVariable` "P"
		,Database [(True, "assume-p", [Con "|-", Var "P"], DollarE)]
		)

	,mmParseFromString "$c var $. $v x $. vx $f var x $." @?=
		Right (ctxEmpty `ctxWithConstant` "var" `ctxWithVariable` "x"
		,Database [(True, "vx", [Con "var", Var "x"], DollarF)]
		)

	,mmParseFromString "$c term $. $v x $. tx $a term x $." @?=
		Right (ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "x"
		,Database [(True, "tx", [Con "term", Var "x"], Axiom [] noDisjoints)]
		)

	,findStatement (case mmParseFromString "$c term $. $v x $. tx $a term x $." of Right (_, db) -> db; _ -> error "imipossible") "tx" @?=
		(True, "tx", [Con "term", Var "x"], Axiom [] noDisjoints)

	,mmParseFromString (unlines
		["$c term $."
		,"$v x $."
		,"ax-tx $a term x $."
		,"th-tx $p term x $= tx $."
		])
	 @?=
		Right (ctxEmpty `ctxWithConstant` "term" `ctxWithVariable` "x"
		,Database
			[(True, "ax-tx", [Con "term", Var "x"], Axiom [] noDisjoints)
			,(True, "th-tx", [Con "term", Var "x"], Theorem [] noDisjoints ["tx"])
			]
		)

	,mmParseFromString "${ $}" @?= Right (ctxEmpty,Database [])

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
		Right (ctxEmpty
		 `ctxWithConstants` ["|-", "(", ")", "->"]
		 `ctxWithVariables` ["P", "Q"]
		,Database
			[(False, "dummy", [Con "|-", Var "P"], DollarF)
			,(False, "min", [Con "|-", Var "P"], DollarE)
			,(False, "maj", [Con "|-", Con "(", Var "P", Con "->", Var "Q", Con ")"], DollarE)
			,(True, "mp", [Con "|-", Var "Q"], Axiom ["min", "maj"] noDisjoints)
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
		Right (ctxEmpty
		 `ctxWithConstants` ["wff", "|-"]
		 `ctxWithVariables` ["P", "Q", "R", "S"]
		,Database
			[(False, "wffp", [Con "wff", Var "P"], DollarF)
			,(False, "wffq", [Con "wff", Var "Q"], DollarF)
			,(False, "wffr", [Con "wff", Var "R"], DollarF)
			,(False, "wffs", [Con "wff", Var "S"], DollarF)
			,(False, "min", [Con "|-", Var "P"], DollarE)
			,(False, "maj", [Con "|-", Var "Q"], DollarE)
			,(True, "mp", [Con "|-", Var "P", Var "R"], Axiom ["wffp", "wffq", "wffr", "min", "maj"] noDisjoints)
			]
		)

	,(case runParser mmpCompressedNumbers ctxEmpty "<test string>" "T UA UB UVA VUA $." of Left _ -> Nothing; Right l -> Just l)
		@?= Just [(19,False),(20,False),(21,False),(120,False),(200,False)]
	,(case runParser mmpCompressedNumbers ctxEmpty "<test string>" "T UA UB UUA UUT UVA $." of Left _ -> Nothing; Right l -> Just l)
		@?= Just [(19,False),(20,False),(21,False),(120,False),(139,False),(140,False)]
	,(case runParser mmpCompressedNumbers ctxEmpty "<test string>" "AAAB\nZB FAACA FAA\nFC DE $." of Left _ -> Nothing; Right l -> Just l)
		@?= Just
		[(0,False),(0,False),(0,False),(1,True ),(1,False)
		,(5,False),(0,False),(0,False),(2,False),(0,False)
		,(5,False),(0,False),(0,False),(5,False),(2,False)
		,(3,False),(4,False)]
	],

	"file-based tests" ~: test
	[do 
		result@(Right (_, db)) <- mmParseFromFile "demo0.mm"
		result @?=
			Right (ctxEmpty
				`ctxWithConstants` ["0","+","=","->","(",")","term","wff","|-"]
				`ctxWithVariables` ["t","r","s","P","Q"]
			,Database
				[(True, "tt",[Con "term",Var "t"],DollarF)
				,(True, "tr",[Con "term",Var "r"],DollarF)
				,(True, "ts",[Con "term",Var "s"],DollarF)
				,(True, "wp",[Con "wff",Var "P"],DollarF)
				,(True, "wq",[Con "wff",Var "Q"],DollarF)
				,(True, "tze",[Con "term",Con "0"],Axiom [] noDisjoints)
				,(True, "tpl",[Con "term",Con "(",Var "t",Con "+",Var "r",Con ")"],Axiom ["tt", "tr"] noDisjoints)
				,(True, "weq",[Con "wff",Var "t",Con "=",Var "r"],Axiom ["tt", "tr"] noDisjoints)
				,(True, "wim",[Con "wff",Con "(",Var "P",Con "->",Var "Q",Con ")"],Axiom ["wp", "wq"] noDisjoints)
				,(True, "a1",[Con "|-",Con "(",Var "t",Con "=",Var "r",Con "->",Con "(",Var "t",Con "=",Var "s",Con "->",Var "r",Con "=",Var "s",Con ")",Con ")"],Axiom ["tt", "tr", "ts"] noDisjoints)
				,(True, "a2",[Con "|-",Con "(",Var "t",Con "+",Con "0",Con ")",Con "=",Var "t"],Axiom ["tt"] noDisjoints)
				,(False, "min",[Con "|-",Var "P"],DollarE)
				,(False, "maj",[Con "|-",Con "(",Var "P",Con "->",Var "Q",Con ")"],DollarE)
				,(True, "mp",[Con "|-",Var "Q"],Axiom ["wp", "wq", "min", "maj"] noDisjoints)
				,(True, "th1",[Con "|-",Var "t",Con "=",Var "t"],Theorem ["tt"] noDisjoints ["tt","tze","tpl","tt","weq","tt","tt","weq","tt","a2","tt","tze","tpl","tt","weq","tt","tze","tpl","tt","weq","tt","tt","weq","wim","tt","a2","tt","tze","tpl","tt","tt","a1","mp","mp"])
				]
			)
		mmComputeTheorem db ["tt"] @?= Right ([Con "term", Var "t"], noDisjoints)
		mmComputeTheorem db ["tt", "tze", "tpl"] @?=
			Right ([Con "term", Con "(", Var "t", Con "+", Con "0", Con ")"], noDisjoints)
		mmVerifiesLabel db "th1" @?= Right ()
		mmVerifiesDatabase db @?= True

	,do
		Right (_, db) <- mmParseFromFile "set-part.mm"
		mmVerifiesLabel db "a1i" @?= Right ()
		mmVerifiesLabel db "a2i" @?= Right ()
		let (_, _, _, Theorem _ _ proof) = findStatement db "id"
		proof @?=
			["wph","wph","wph","wi","wi"
			,"wph","wph","wi"
			,"wph","wph","ax-1","wph"
			,"wph","wph","wi"
			,"wph","wph"
			,"wph","wph","wi"
			,"ax-1","a2i","ax-mp"
			]
		mmVerifiesDatabase db @?= True

	,do
		Right (_, db) <- mmParseFromFile "set-part2.mm"
		let (_, _, _, Theorem _ _ proof) = findStatement db "cbvex"
		proof @?=
			["wph","wn","vx","wal","wn","wps","wn","vy","wal","wn","wph","vx","wex","wps","vy","wex","wph","wn","vx","wal","wps"
			,"wn","vy","wal","wph","wn","wps","wn","vx","vy","wph","vy","cbvex.1","hbne","wps","vx","cbvex.2","hbne","vx"
			,"vy","weq","wph","wps","cbvex.3","negbid","cbval","negbii","wph","vx","df-ex","wps","vy","df-ex"
			,"3bitr4"
			]
		let (_, _, _, Theorem _ disjoints _) = findStatement db "ax17eq"
		disjoints @?= Disjoints [("x", "z"), ("y", "z")]
		mmVerifiesLabel db "ax17eq" @?= Right ()
		mmComputeTheorem db
			["vz","vx","weq","vz","wal","vz","vy","weq","vz","wal"
			,"vx","vy","weq","vx","vy","weq","vz","wal","wi","vx"
			,"vy","vz","ax-12","vx","vy","weq","vz","vx","ax-16","vx"
			,"vy","weq","vz","vy","ax-16","pm2.61ii"
			]
			@?= Right ([Con "|-",Con "(",Var "x",Con "=",Var "y",Con "->",Con "A.",Var "z",Var "x",Con "=",Var "y",Con ")"], Disjoints [("x","z"), ("y","z")])

	,do
		Right (_, db) <- mmParseFromFile "set-part3.mm"
		let (_, _, _, Theorem _ disjoints _) = findStatement db "a16g"
		disjoints @?= Disjoints [("x", "y")]
		mmComputeTheorem db
			["vz","vx","weq","vz","wal","vx","vy","weq","vx","wal"
			,"wph","wph","vz","wal","wi","vx","vy","weq","vx","wal"
			,"vz","vx","weq","vz","vx","vy","vz","eq5","vx","vy"
			,"weq","vx","wal","vx","vz","weq","vz","vx","weq","vx"
			,"vy","weq","vx","wal","vx","vz","weq","vx","vz","weq"
			,"wn","vx","wal","vx","vz","ax9a","vx","vz","weq","wn"
			,"vx","vy","ax-16","mt3i","vx","vz","eqcom","syl","19.21ai","vx"
			,"vy","weq","vx","wal","wph","wph","vx","wal","vz","vx"
			,"weq","vz","wal","wph","vz","wal","wph","vx","vy","ax-16"
			,"wph","wph","vz","vx","vz","vx","weq","vz","wal","wph"
			,"idd","del35","syl9r","mpcom"
			]
			@?= Right ([Con "|-",Con "(",Con "A.",Var "x",Var "x",Con "=",Var "y",Con "->",Con "(",Var "ph",Con "->",Con "A.",Var "z",Var "ph",Con ")",Con ")"], Disjoints [("x","y")])
		mmVerifiesLabel db "a16g" @?= Right ()

	,do
		Right (_, db) <- mmParseFromFile "set-part4.mm"
		let (_, _, _, Theorem _ disjoints _) = findStatement db "ddeeq1"
		disjoints @?= Disjoints [("x","z")]
		mmComputeTheorem db
			["vw","vz","weq","vy","vz","weq","vx","vy","vw","vw"
			,"vz","weq","vx","ax-17","vw","vy","vz","a8b","ddelim"
			]
			@?= Right (
				[Con "|-",Con "(",Con "-.",Con "A.",Var "x",Var "x",Con "=",Var "y"
					,Con "->",Con "(",Var "y",Con "=",Var "z"
						,Con "->",Con "A.",Var "x",Var "y",Con "=",Var "z"
					,Con ")"
				,Con ")"
				], Disjoints [("x","z")])
		mmVerifiesLabel db "ddeeq1" @?= Right ()
		let (_, _, _, Theorem _ disjoints2 _) = findStatement db "sbal2"
		disjoints2 @?= Disjoints [("x","z"),("y","z")]
		mmComputeTheorem db
			["vx","vy","weq","vx","wal","wn","vy","vz","weq","wph","vx","wal","wi","vy","wal","vy","vz","weq","wph","wi","vy"
			,"wal","vx","wal","wph","vx","wal","vy","vz","wsb","wph","vy","vz","wsb","vx","wal","vx","vy","weq","vx","wal","wn"
			,"vy","vz","weq","wph","wi","vx","wal","vy","wal","vy","vz","weq","wph","vx","wal","wi","vy","wal","vy","vz","weq"
			,"wph","wi","vy","wal","vx","wal","vx","vy","weq","vx","wal","wn","vy","vz","weq","wph","wi","vx","wal","vy","vz"
			,"weq","wph","vx","wal","wi","vy","vx","vy","vy","eq6","vx","vy","weq","vx","wal","wn","vy","vz","weq","vy","vz","weq"
			,"vx","wal","wi","vx","wal","vy","vz","weq","wph","wi","vx","wal","vy","vz","weq","wph","vx","wal","wi","wb","vy","vz"
			,"weq","vy","vz","weq","vx","wal","wi","vx","wal","vx","vy","vx","vx","vy","weq","vx","wal","wn","vy","vz","weq","vy"
			,"vz","weq","vx","wal","wi","vx","vx","vy","vz","ddeeq1","19.20i","eq6s","vy","vz","weq","wph","vx","19.21g"
			,"syl","biald","vy","vz","weq","wph","wi","vy","vx","alcom","syl5rbbr","wph","vx","wal","vy","vz","sb6","wph"
			,"vy","vz","wsb","vy","vz","weq","wph","wi","vy","wal","vx","wph","vy","vz","sb6","bial","3bitr4g"
			]
			@?= Right (
				[Con "|-",Con "(",Con "-.",Con "A.",Var "x",Var "x",Con "=",Var "y"
					,Con "->",Con "(",Con "[",Var "z",Con "/",Var "y",Con "]",Con "A.",Var "x",Var "ph"
						,Con "<->",Con "A.",Var "x",Con "[",Var "z",Con "/",Var "y",Con "]",Var "ph"
					,Con ")"
				,Con ")"
 				], Disjoints [("x","z"),("y","z")])

	,do
		Right (_, db) <- mmParseFromFile "peano.mm"
		findStatement db "binop_plus" @?= (True, "binop_plus", [Con "BINOP", Con "+"], Axiom [] noDisjoints)
		mmVerifiesDatabase db @?= True

	,do
		Right (_, db) <- mmParseFromFile "ql-part.mm"
		mmVerifiesLabel db "ska2" @?= Right ()
	]

	]


ctxWithConstant :: Context -> String -> Context
ctx `ctxWithConstant` c = ctx `ctxWithConstants` [c]

ctxWithVariable :: Context -> String -> Context
ctx `ctxWithVariable` v = ctx `ctxWithVariables` [v]

noDisjoints :: Disjoints
noDisjoints = Disjoints []
