{-
TODO list, roughly in order of preference:

 - functionality: verifier should give more detail on what went wrong: no call
   to "error" anymore on any input file (and add tests to check wrong input).

 - readability: make (Context, Database) the state of GenParser, instead of
   passing it around all the time

 - performance: use Data.Map for the database

 - performance: store active variables etc. in the context, instead of looking
   them up every time

 - readability: tuck special try/mmpSeparator stuff in new combinator.

 - functionality: support for include files ($[ ... $])

 - modularity: find some trick to export functions to HmmTest only.

-}




module Hmm
	(Context(Context)
	,ctxEmpty,ctxWithConstants,ctxWithVariables,ctxWithDisjoints
	,Database(Database)
	,Statement
	,StatementInfo(DollarE, DollarF, Axiom, Theorem)
	,Symbol(Var,Con)
	,Disjoints(Disjoints)
	,findStatement
	,mmParseFromFile,mmParseFromString
	,mmComputeTheorem,mmVerifiesProof,mmVerifiesLabel,mmVerifiesDatabase,mmVerifiesAll

	,mmpCompressedNumbers

	,allPairs
	)

where

import Text.ParserCombinators.Parsec
import Data.List(sort)
import Data.Char(isSpace,isAscii,isControl)



data Database = Database [Statement]
	deriving (Eq, Show)

type Statement = (Bool, Label, Expression, Disjoints, StatementInfo)

data StatementInfo = DollarE | DollarF | Axiom [Label] | Theorem [Label] Proof
	deriving (Eq, Show, Ord)

--NOTE: a Disjoints never may contain a pair of identical strings!
newtype Disjoints = Disjoints [(String,String)]
	deriving (Show, Ord)

instance Eq Disjoints where
	Disjoints d1 == Disjoints d2 = sort (map sortPair d1) == sort (map sortPair d2)

data Symbol = Var String | Con String
	deriving (Eq, Show, Ord)

type Expression = [Symbol]
type Label = String
type Proof = [String]

dbEmpty :: Database
dbEmpty = Database []

dbWith:: Database -> Database -> Database
Database ss1 `dbWith` Database ss2 = Database (ss1++ss2)

selectMandatoryDisjointsFor :: Expression -> Database -> Context -> Disjoints
selectMandatoryDisjointsFor symbols db ctx = Disjoints ds
	where
		ds = [d | d@(x, y) <- case ctxDisjoints ctx of Disjoints ds2 -> ds2, x `elem` mandatoryVars, y `elem` mandatoryVars]
		mandatoryVars = activeDollarEVars db ++ varsOf symbols

selectMandatoryLabelsForVarsOf :: Expression -> Database -> [Label]
selectMandatoryLabelsForVarsOf symbols db@(Database ss) =
	[lab |
		(act, lab, syms, _, info) <- ss,
		act,
		case info of
			DollarE -> True
			DollarF -> let [Con _c, Var v] = syms in
				v `elem` (activeDollarEVars db ++ varsOf symbols)
			_ -> False
	]

activeDollarEVars :: Database -> [String]
activeDollarEVars (Database ss) =
	concat [varsOf syms |
		(act, _, syms, _, info) <- ss,
		act,
		info == DollarE
	]

varsOf :: Expression -> [String]
varsOf [] = []
varsOf (Var v : rest) = v : varsOf rest
varsOf (Con _c : rest) = varsOf rest


isAssertion :: Statement -> Bool
isAssertion (_, _, _, _, Theorem _ _) = True
isAssertion (_, _, _, _, Axiom _) = True
isAssertion _ = False



data Context = Context {ctxConstants::[String], ctxVariables::[String], ctxDisjoints::Disjoints}
	deriving Show

instance Eq Context where
	c1 == c2 =
		sort (ctxConstants c1) == sort (ctxConstants c2)
		&& sort (ctxVariables c1) == sort (ctxVariables c2)

ctxEmpty :: Context
ctxEmpty = Context {ctxConstants = [], ctxVariables = [], ctxDisjoints = Disjoints []}

ctxWithConstants :: Context -> [String] -> Context
ctx `ctxWithConstants` cs = ctx {ctxConstants = cs ++ ctxConstants ctx}

ctxWithVariables :: Context -> [String] -> Context
ctx `ctxWithVariables` vs = ctx {ctxVariables = vs ++ ctxVariables ctx}

ctxWithDisjoints :: Context -> [(String, String)] -> Context
ctx `ctxWithDisjoints` ds = ctx {ctxDisjoints = Disjoints (ds ++ case ctxDisjoints ctx of Disjoints d -> d)}





mmParseFromFile:: String -> IO (Context, Database)
mmParseFromFile path = do
		contents <- readFile path
		return (mmParse path contents)

mmParseFromString :: String -> (Context, Database)
mmParseFromString s = mmParse "<string>" s

mmParse :: String -> String -> (Context, Database)
mmParse source s =
	case parse mmpDatabase source s of
		Left err -> error $ show err
		Right result -> result


mmpDatabase :: Parser (Context, Database)
mmpDatabase = do
		try mmpSeparator <|> return ()
		(ctx, db) <- mmpStatements (ctxEmpty, dbEmpty)
		eof
		return (ctx, db)

mmpStatements :: (Context, Database) -> Parser (Context, Database)
mmpStatements (ctx, db) =
		do
			(ctx2, dbstat) <- mmpStatement (ctx, db)
			let db2 = db `dbWith` dbstat
			(do
				mmpSeparator
				(ctx3, dbstats) <- mmpStatements (ctx2, db2)
				return (ctx3, dbstat `dbWith` dbstats)
			 <|> return (ctx2, dbstat))
		<|> return (ctx, dbEmpty)

mmpStatement :: (Context, Database) -> Parser (Context, Database)
mmpStatement (ctx, db) =
		(   mmpConstants (ctx, db)
		<|> mmpVariables (ctx, db)
		<|> mmpRestrictions (ctx, db)
		<|> mmpDollarE (ctx, db)
		<|> mmpDollarF (ctx, db)
		<|> mmpAxiom (ctx, db)
		<|> mmpTheorem (ctx, db)
		<|> mmpBlock (ctx, db)
		) <?> "statement"

mmpSeparator :: Parser ()
mmpSeparator = do
		many1 ((space >> return ()) <|> mmpComment)
		return ()

mmpComment :: Parser ()
mmpComment = do
		try (string "$(")
		manyTill anyChar (try (space >> string "$)"))
		return ()
	    <?> "comment"

mmpConstants :: (Context, Database) -> Parser (Context, Database)
mmpConstants (ctx, _db) = do
		mmpTryUnlabeled "$c"
		mmpSeparator
		cs <- mmpSepListEndBy mmpIdentifier "$."
		return (ctx `ctxWithConstants` cs, Database [])

mmpVariables :: (Context, Database) -> Parser (Context, Database)
mmpVariables (ctx, _db) = do
		mmpTryUnlabeled "$v"
		mmpSeparator
		cs <- mmpSepListEndBy mmpIdentifier "$."
		return (ctx `ctxWithVariables` cs, Database [])

mmpRestrictions :: (Context, Database) -> Parser (Context, Database)
mmpRestrictions (ctx, _db) = do
		mmpTryUnlabeled "$d"
		mmpSeparator
		d <- mmpSepListEndBy mmpIdentifier "$."
		let pairs = allPairs d
		if samePairs pairs
			then error ("found same variable twice in $d " ++ show d)
			else return (ctx `ctxWithDisjoints` pairs, Database [])

mmpDollarE :: (Context, Database) -> Parser (Context, Database)
mmpDollarE (ctx, _db) = do
		lab <- mmpTryLabeled "$e"
		mmpSeparator
		ss <- mmpSepListEndBy mmpIdentifier "$."
		return (ctx, Database [(True, lab, mapSymbols ctx ss, Disjoints [], DollarE)])

mmpDollarF :: (Context, Database) -> Parser (Context, Database)
mmpDollarF (ctx, _db) = do
		lab <- mmpTryLabeled "$f"
		mmpSeparator
		c <- mmpIdentifier
		mmpSeparator
		v <- mmpIdentifier
		mmpSeparator
		string "$."
		return (ctx, Database [(True, lab, mapSymbols ctx [c, v], Disjoints [], DollarF)])

mmpAxiom :: (Context, Database) -> Parser (Context, Database)
mmpAxiom (ctx, db) = do
		lab <- mmpTryLabeled "$a"
		mmpSeparator
		ss <- mmpSepListEndBy mmpIdentifier "$."
		let symbols = mapSymbols ctx ss
		return (ctx, Database [(True, lab, symbols, selectMandatoryDisjointsFor symbols db ctx, Axiom (selectMandatoryLabelsForVarsOf symbols db))])

mmpTheorem :: (Context, Database) -> Parser (Context, Database)
mmpTheorem (ctx, db) = do
		lab <- mmpTryLabeled "$p"
		mmpSeparator
		ss <- mmpSepListEndBy mmpIdentifier "$="
		mmpSeparator
		let symbols = mapSymbols ctx ss
		let mandatoryLabels = selectMandatoryLabelsForVarsOf symbols db
		ps <- (mmpUncompressedProof <|> mmpCompressedProof db mandatoryLabels)
		return (ctx, Database [(True, lab, symbols, selectMandatoryDisjointsFor symbols db ctx, Theorem mandatoryLabels ps)])

mmpUncompressedProof :: Parser Proof
mmpUncompressedProof = do
		mmpSepListEndBy mmpLabel "$."

mmpCompressedProof :: Database -> [Label] -> Parser Proof
mmpCompressedProof db mandatoryLabels = do
		string "("
		mmpSeparator
		assertionLabels <- mmpSepListEndBy mmpLabel ")"
		mmpSeparator
		markedNumbers <- mmpCompressedNumbers
		return (createProof assertionLabels markedNumbers)
	where
		createProof :: [Label] -> [(Int,Bool)] -> Proof
		createProof assertionLabels markedNumbers = proof
			where
				proof :: Proof
				proof = proof' markedNumbers ([], [], [])

				-- The meaning of the accumulated arguments:
				-- marked: the 1st, 2nd, ... marked subproofs
				-- subs:   the subproofs ending at the 1st, 2nd, ... number in the list
				-- p:      the proof resulting from all markedNumbers processed so far
				proof' :: [(Int, Bool)] -> ([Proof], [Proof], Proof) -> Proof
				proof' [] (_, _, p) = p
				proof' ((n, mark):rest) (marked, subs, p) = proof' rest (newMarked, newSubs, newP)
					where
						-- meaning !! n =
						--	(the subproof associated with number n
						--	,the number of proof steps that it pops from the proof stack
						--	)
						meaning :: [(Proof, Int)]
						meaning =
							map (\lab -> ([lab], 0)) mandatoryLabels
							++ map (\lab -> ([lab], length (getHypotheses (findStatement db lab))))
								assertionLabels
							++ zip marked (repeat 0)

						newSteps :: Proof
						newSteps = fst (meaning !! n)

						newSub :: Proof
						newSub = concat ((reverse . take (snd (meaning !! n)) . reverse) subs)
							++ newSteps

						newMarked :: [Proof]
						newMarked = if mark then marked ++ [newSub] else marked

						newSubs :: [Proof]
						newSubs = subs ++ [newSub]

						newP :: Proof
						newP = p ++ newSteps

mmpCompressedNumbers :: Parser [(Int, Bool)]
mmpCompressedNumbers = manyTill
		(do
			n <- mmpCompressedNumber
			marked <- try ((try mmpSeparator <|> return ()) >> (((oneOf "Z") >> return True) <|> return False))
			return (n, marked)
		)
		(try ((try mmpSeparator <|> return ()) >> string "$."))

mmpCompressedNumber :: Parser Int
mmpCompressedNumber = do
		base20 <- many (do
			c <- try ((try mmpSeparator <|> return ()) >> satisfy (\c -> 'U' <= c && c <= 'Y')) <?> "U...Y"
			return (fromEnum c - fromEnum 'U')
			)
		base5 <- do
			c <- try ((try mmpSeparator <|> return ()) >> satisfy (\c -> 'A' <= c && c <= 'T')) <?> "A...T"
			return (fromEnum c - fromEnum 'A')
		return (foldr (\x y -> x * 20 + y) base5 base20)

mmpBlock :: (Context, Database) -> Parser (Context, Database)
mmpBlock (ctx, db) = do
		mmpTryUnlabeled "${"
		mmpSeparator
		(ctx2, db2) <- mmpStatements (ctx, db)
		string "$}"
		return (ctx2, deactivateNonAssertions db2)
	where
		deactivateNonAssertions :: Database -> Database
		deactivateNonAssertions (Database ss) =
			Database
				[(newact, lab, symbols, disjoints, info) |
					stat@(act, lab, symbols, disjoints, info) <- ss,
					let newact = if isAssertion stat then act else False
				]

mmpTryUnlabeled :: String -> Parser ()
mmpTryUnlabeled keyword = (try (string keyword) >> return ()) <?> (keyword ++ " keyword")

mmpTryLabeled :: String -> Parser Label
mmpTryLabeled keyword = (try $ do
				lab <- mmpLabel
				mmpSeparator
				string keyword
				return lab
			) <?> ("labeled " ++ keyword ++ " keyword")

mmpSepListEndBy :: Parser a -> String -> Parser [a]
mmpSepListEndBy p end = manyTill (do {s <- p; mmpSeparator; return s}) (try (string end))

mmpIdentifier :: Parser String
mmpIdentifier = many1 (satisfy isMathSymbolChar) <?> "math symbol"

mmpLabel :: Parser Label
mmpLabel = many1 (alphaNum <|> oneOf "-_.")

isMathSymbolChar :: Char -> Bool
isMathSymbolChar c = isAscii c && not (isSpace c) && not (isControl c)

mapSymbols :: Context -> [String] -> Expression
mapSymbols ctx = map $ \s ->
			if s `elem` ctxConstants ctx then Con s
			else if s `elem` ctxVariables ctx then Var s
			else error ("Unknown math symbol " ++ s)


mmComputeTheorem :: Database -> Proof -> Maybe Expression
mmComputeTheorem db proof = case foldProof db proof combine of [th] -> Just th; _ -> Nothing
	where
		combine :: Statement -> [(Label, Expression)] -> Expression
		combine stat labSymsList = newSyms
			where
				(_, _, syms, disjoints, _) = stat
				subst = case unify (map (\(lab, ss) -> let (expr, d) = findExpressionAndDisjoints db lab in (expr, d, ss)) labSymsList) of
					Just s -> s
					Nothing -> error "could not unify"
				newSyms = case applySubstitution disjoints subst syms of
					Just expr -> expr
					_ -> error ("disjoints violation: disjoints=" ++ show disjoints ++ ", subst=" ++ show subst ++ ", syms=" ++ show syms)

foldProof :: Database -> Proof -> (Statement -> [(Label, a)] -> a) -> [a]
foldProof db labs f = foldProof' db labs f []

foldProof' :: Database -> Proof -> (Statement -> [(Label, a)] -> a) -> [a] -> [a]
foldProof' _ [] _ stack = stack
foldProof' db (lab:labs) f stack = foldProof' db labs f (newTop:poppedStack)
	where
		stat = findStatement db lab
		hyps = getHypotheses stat
		nHyps = length hyps
		poppedStack = drop nHyps stack
		newTop = f stat (zip hyps (reverse (take nHyps stack)))


type Substitution = [(String, Expression)]

unify :: [(Expression, Disjoints, Expression)] -> Maybe Substitution
unify tuples = unify' tuples []

unify' :: [(Expression, Disjoints, Expression)] -> Substitution -> Maybe Substitution
unify' [] subst = Just subst
unify' (([Con c1, Var v], _, Con c2 : syms):tuples) subst | c1 == c2 && lookup v subst == Nothing =
	unify' tuples ((v, syms) : subst)
unify' ((fromSyms, d, toSyms) : tuples) subst | applySubstitution d subst fromSyms == Just toSyms =
	unify' tuples subst
unify' _ _ = Nothing

applySubstitution :: Disjoints -> Substitution -> Expression -> Maybe Expression
applySubstitution d subst expr = if checkSubstitution d subst then Just (applySubstitution' subst expr) else Nothing

applySubstitution' :: Substitution -> Expression -> Expression
applySubstitution' _ [] = []
applySubstitution' subst (Con c : rest) = Con c : applySubstitution' subst rest
applySubstitution' subst (Var v : rest) =
	(case lookup v subst of Just ss -> ss; Nothing -> [Var v])
	++ applySubstitution' subst rest

checkSubstitution :: Disjoints -> Substitution -> Bool
checkSubstitution (Disjoints disjoints) substitution =
		and [ [(v, w) | v <- varsOf e, w <- varsOf f] `subset` disjoints |
			((x, e), (y, f)) <- allPairs substitution,
			(x, y) `elem` disjoints
		]
			

mmVerifiesLabel :: Database -> Label -> Bool
mmVerifiesLabel db lab = mmVerifiesProof db proof
	where
		stat = findStatement db lab
		(_, _, _, _, Theorem _ proof) = stat

mmVerifiesProof :: Database -> Proof -> Bool
mmVerifiesProof db proof = case mmComputeTheorem db proof of Just _ -> True; Nothing -> False

mmVerifiesAll :: Database -> [(Label, Bool)]
mmVerifiesAll db@(Database stats) = map (\(lab, proof) -> (lab, mmVerifiesProof db proof)) (selectProofs stats)
	where
		selectProofs :: [Statement] -> [(Label, Proof)]
		selectProofs [] = []
		selectProofs ((_, lab, _, _, Theorem _ proof):rest) = (lab, proof) : selectProofs rest
		selectProofs (_:rest) = selectProofs rest

mmVerifiesDatabase :: Database -> Bool
mmVerifiesDatabase db = all snd (mmVerifiesAll db)

findStatement :: Database -> Label -> Statement
findStatement (Database []) lab = error $ "statement labeled " ++ lab ++ " not found"
findStatement (Database (stat@(_, lab2, _, _, _):rest)) lab
	| lab == lab2	= stat
	| True		= findStatement (Database rest) lab

findExpressionAndDisjoints :: Database -> Label -> (Expression, Disjoints)
findExpressionAndDisjoints db lab = (syms, disjoints) where (_, _, syms, disjoints, _) = findStatement db lab

getHypotheses :: Statement -> [Label]
getHypotheses (_, _, _, _, Axiom hyp) = hyp
getHypotheses (_, _, _, _, Theorem hyp _) = hyp
getHypotheses _ = []


allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (a:as) = [(a, a2) | a2 <- as] ++ allPairs as

samePairs :: Eq a => [(a,a)] -> Bool
samePairs = any (\(x,y) -> x == y)

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (x,y)	| x <= y = (x,y)
		| True   = (y,x)

subset :: Eq a => [a] -> [a] -> Bool
subset l m = and [x `elem` m | x <- l]
