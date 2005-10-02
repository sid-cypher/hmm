module Hmm
	(Context(Context)
	,ctxEmpty,ctxWithConstants,ctxWithVariables
	,Database(Database)
	,Statement
	,StatementInfo(DollarE, DollarF, Axiom, Theorem)
	,Symbol(Var,Con)
	,mmParseFromFile,mmParseFromString
	)

where

import Text.ParserCombinators.Parsec
import Data.List(sort)
import Data.Char(isSpace,isAscii,isControl)



data Database = Database [Statement]
	deriving (Eq, Show)

type Statement = (Bool, String, [Symbol], StatementInfo)

data StatementInfo = DollarE | DollarF | Axiom [String] | Theorem [String] [String]
	deriving (Eq, Show, Ord)

data Symbol = Var String | Con String
	deriving (Eq, Show, Ord)

dbEmpty :: Database
dbEmpty = Database []

dbWith:: Database -> Database -> Database
Database ss1 `dbWith` Database ss2 = Database (ss1++ss2)


selectMandatoryLabelsForVarsOf :: [Symbol] -> Database -> [String]
selectMandatoryLabelsForVarsOf symbols db@(Database ss) =
	[lab |
		(act, lab, syms, info) <- ss,
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
		(act, _, syms, info) <- ss,
		act,
		info == DollarE
	]

varsOf :: [Symbol] -> [String]
varsOf [] = []
varsOf (Var v : rest) = v : varsOf rest
varsOf (Con _c : rest) = varsOf rest


isAssertion :: Statement -> Bool
isAssertion (_, _, _, Theorem _ _) = True
isAssertion (_, _, _, Axiom _) = True
isAssertion _ = False



data Context = Context {ctxConstants::[String], ctxVariables::[String]}
	deriving Show

instance Eq Context where
	c1 == c2 =
		sort (ctxConstants c1) == sort (ctxConstants c2)
		&& sort (ctxVariables c1) == sort (ctxVariables c2)

ctxEmpty :: Context
ctxEmpty = Context {ctxConstants = [], ctxVariables = []}

ctxWithConstants :: Context -> [String] -> Context
ctx `ctxWithConstants` cs = ctx {ctxConstants = cs ++ ctxConstants ctx}

ctxWithVariables :: Context -> [String] -> Context
ctx `ctxWithVariables` vs = ctx {ctxVariables = vs ++ ctxVariables ctx}





mmParseFromFile:: String -> IO (Context, Database)
mmParseFromFile path = do
		contents <- readFile path
		return (mmParseFromString contents)

mmParseFromString :: String -> (Context, Database)
mmParseFromString s =
	case parse mmpDatabase "<string>" s of
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
mmpConstants (ctx, db) = do
		mmpTryUnlabeled "$c"
		mmpSeparator
		cs <- mmpIdentifiersThen "$."
		return (ctx `ctxWithConstants` cs, db)

mmpVariables :: (Context, Database) -> Parser (Context, Database)
mmpVariables (ctx, db) = do
		mmpTryUnlabeled "$v"
		mmpSeparator
		cs <- mmpIdentifiersThen "$."
		return (ctx `ctxWithVariables` cs, db)

mmpDollarE :: (Context, Database) -> Parser (Context, Database)
mmpDollarE (ctx, _db) = do
		lab <- mmpTryLabeled "$e"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		return (ctx, Database [(True, lab, mapSymbols ctx ss, DollarE)])

mmpDollarF :: (Context, Database) -> Parser (Context, Database)
mmpDollarF (ctx, _db) = do
		lab <- mmpTryLabeled "$f"
		mmpSeparator
		c <- mmpIdentifier
		mmpSeparator
		v <- mmpIdentifier
		mmpSeparator
		string "$."
		return (ctx, Database [(True, lab, mapSymbols ctx [c, v], DollarF)])

mmpAxiom :: (Context, Database) -> Parser (Context, Database)
mmpAxiom (ctx, db) = do
		lab <- mmpTryLabeled "$a"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		let symbols = mapSymbols ctx ss
		return (ctx, Database [(True, lab, symbols, Axiom (selectMandatoryLabelsForVarsOf symbols db))])

mmpTheorem :: (Context, Database) -> Parser (Context, Database)
mmpTheorem (ctx, db) = do
		lab <- mmpTryLabeled "$p"
		mmpSeparator
		ss <- mmpIdentifiersThen "$="
		mmpSeparator
		ps <- mmpIdentifiersThen "$."
		let symbols = mapSymbols ctx ss
		return (ctx, Database [(True, lab, symbols, Theorem (selectMandatoryLabelsForVarsOf symbols db) ps)])

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
				[(newact, lab, symbols, info) |
					stat@(act, lab, symbols, info) <- ss,
					let newact = if isAssertion stat then act else False
				]

mmpTryUnlabeled :: String -> Parser ()
mmpTryUnlabeled keyword = (try (string keyword) >> return ()) <?> (keyword ++ " keyword")

mmpTryLabeled :: String -> Parser String
mmpTryLabeled keyword = (try $ do
				lab <- mmpIdentifier
				mmpSeparator
				string keyword
				return lab
			) <?> ("labeled " ++ keyword ++ " keyword")

mmpIdentifiersThen :: String -> Parser [String]
mmpIdentifiersThen end = manyTill (do {s<-mmpIdentifier; mmpSeparator; return s}) (try (string end))

mmpIdentifier :: Parser String
mmpIdentifier = many1 (satisfy isMathSymbolChar) <?> "math symbol"

isMathSymbolChar :: Char -> Bool
isMathSymbolChar c = isAscii c && not (isSpace c) && not (isControl c)

mapSymbols :: Context -> [String] -> [Symbol]
mapSymbols ctx = map $ \s ->
			if s `elem` ctxConstants ctx then Con s
			else if s `elem` ctxVariables ctx then Var s
			else error ("Unknown math symbol " ++ s)
