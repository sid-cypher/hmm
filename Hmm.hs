module Hmm
	(Context(Context)
	,ctxEmpty,ctxWithConstants,ctxWithVariables
	,Database(Database)
	,Statement
	,StatementInfo(DollarE, DollarF, Axiom, Theorem)
	,Symbol(Var,Con)
	,mmParseFromString
	)

where

import Text.ParserCombinators.Parsec
import Data.List(sort)
import Data.Char(isSpace,isAscii,isControl)



data Database = Database [Statement] [Statement]
	deriving Show

instance Eq Database where
	Database iss1 ass1 == Database iss2 ass2 =
		sort iss1 == sort iss2 && sort ass1 == sort ass2

type Statement = (String, [Symbol], StatementInfo)

data StatementInfo = DollarE | DollarF | Axiom | Theorem [String]
	deriving (Eq, Show, Ord)

data Symbol = Var String | Con String
	deriving (Eq, Show, Ord)

dbEmpty = Database [] []

Database iss1 ass1 `dbWith` Database iss2 ass2 = Database (iss1++iss2) (ass1++ass2)


isAssertion :: Statement -> Bool
isAssertion (_, _, Theorem _) = True
isAssertion (_, _, Axiom) = True
isAssertion _ = False



data Context = Context {ctxConstants::[String], ctxVariables::[String]}
	deriving Show

instance Eq Context where
	d1 == d2 =
		sort (ctxConstants d1) == sort (ctxConstants d2)
		&& sort (ctxVariables d1) == sort (ctxVariables d2)

ctxEmpty = Context {ctxConstants = [], ctxVariables = []}

ctx `ctxWithConstants` cs = ctx {ctxConstants = cs ++ ctxConstants ctx}

ctx `ctxWithVariables` vs = ctx {ctxVariables = vs ++ ctxVariables ctx}






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
			(do
				mmpSeparator
				(ctx3, dbstats) <- mmpStatements (ctx2, dbstat)
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
mmpDollarE (ctx, db) = do
		label <- mmpTryLabeled "$e"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		return (ctx, Database [] [(label, mapSymbols ctx ss, DollarE)])

mmpDollarF :: (Context, Database) -> Parser (Context, Database)
mmpDollarF (ctx, db) = do
		label <- mmpTryLabeled "$f"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		return (ctx, Database [] [(label, mapSymbols ctx ss, DollarF)])

mmpAxiom :: (Context, Database) -> Parser (Context, Database)
mmpAxiom (ctx, db) = do
		label <- mmpTryLabeled "$a"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		return (ctx, Database [] [(label, mapSymbols ctx ss, Axiom)])

mmpTheorem :: (Context, Database) -> Parser (Context, Database)
mmpTheorem (ctx, db) = do
		label <- mmpTryLabeled "$p"
		mmpSeparator
		ss <- mmpIdentifiersThen "$="
		mmpSeparator
		ps <- mmpIdentifiersThen "$."
		return (ctx, Database [] [(label, mapSymbols ctx ss, Theorem ps)])

mmpBlock :: (Context, Database) -> Parser (Context, Database)
mmpBlock (ctx, db) = do
		mmpTryUnlabeled "${"
		mmpSeparator
		(ctx2, db2) <- mmpStatements (ctx, db)
		string "$}"
		let Database inactiveStatements activeStatements = db2
		let (nonAssertions, assertions) = statSplit activeStatements
		return (ctx2, Database (inactiveStatements++nonAssertions) assertions)

statSplit :: [Statement] -> ([Statement], [Statement])
statSplit [] = ([],[])
statSplit (stat:rest)
	| isAssertion stat	= (restNonAssertions, stat:restAssertions)
	| True			= (stat:restNonAssertions, restAssertions)
	where (restNonAssertions, restAssertions) = statSplit rest

mmpTryUnlabeled :: String -> Parser ()
mmpTryUnlabeled keyword = (try (string keyword) >> return ()) <?> (keyword ++ " keyword")

mmpTryLabeled :: String -> Parser String
mmpTryLabeled keyword = (try $ do
				label <- mmpIdentifier
				mmpSeparator
				string keyword
				return label
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
