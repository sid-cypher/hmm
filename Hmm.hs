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



data Database = Database [Statement] [Statement]
	deriving (Eq, Show)

type Statement = (String, [Symbol], StatementInfo)

data StatementInfo = DollarE | DollarF | Axiom [String] | Theorem [String] [String]
	deriving (Eq, Show, Ord)

data Symbol = Var String | Con String
	deriving (Eq, Show, Ord)

dbEmpty = Database [] []

Database iss1 ass1 `dbWith` Database iss2 ass2 = Database (iss1++iss2) (ass1++ass2)


isAssertion :: Statement -> Bool
isAssertion (_, _, Theorem _ _) = True
isAssertion (_, _, Axiom _) = True
isAssertion _ = False



data Context = Context {ctxConstants::[String], ctxVariables::[String]}
	deriving Show

instance Eq Context where
	c1 == c2 =
		sort (ctxConstants c1) == sort (ctxConstants c2)
		&& sort (ctxVariables c1) == sort (ctxVariables c2)

ctxEmpty = Context {ctxConstants = [], ctxVariables = []}

ctx `ctxWithConstants` cs = ctx {ctxConstants = cs ++ ctxConstants ctx}

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
		(ctx, db) <- mmpStatements ctxEmpty
		eof
		return (ctx, db)

mmpStatements :: Context -> Parser (Context, Database)
mmpStatements ctx =
		do
			(ctx2, dbstat) <- mmpStatement ctx
			(do
				mmpSeparator
				(ctx3, dbstats) <- mmpStatements ctx2
				return (ctx3, dbstat `dbWith` dbstats)
			 <|> return (ctx2, dbstat))
		<|> return (ctx, dbEmpty)

mmpStatement :: Context -> Parser (Context, Database)
mmpStatement ctx =
		(   mmpConstants ctx
		<|> mmpVariables ctx
		<|> mmpDollarE ctx
		<|> mmpDollarF ctx
		<|> mmpAxiom ctx
		<|> mmpTheorem ctx
		<|> mmpBlock ctx
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

mmpConstants :: Context -> Parser (Context, Database)
mmpConstants ctx = do
		mmpTryUnlabeled "$c"
		mmpSeparator
		cs <- mmpIdentifiersThen "$."
		return (ctx `ctxWithConstants` cs, dbEmpty)

mmpVariables :: Context -> Parser (Context, Database)
mmpVariables ctx = do
		mmpTryUnlabeled "$v"
		mmpSeparator
		cs <- mmpIdentifiersThen "$."
		return (ctx `ctxWithVariables` cs, dbEmpty)

mmpDollarE :: Context -> Parser (Context, Database)
mmpDollarE ctx = do
		label <- mmpTryLabeled "$e"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		return (ctx, Database [] [(label, mapSymbols ctx ss, DollarE)])

mmpDollarF :: Context -> Parser (Context, Database)
mmpDollarF ctx = do
		label <- mmpTryLabeled "$f"
		mmpSeparator
		c <- mmpIdentifier
		mmpSeparator
		v <- mmpIdentifier
		mmpSeparator
		string "$."
		return (ctx, Database [] [(label, mapSymbols ctx [c, v], DollarF)])

mmpAxiom :: Context -> Parser (Context, Database)
mmpAxiom ctx = do
		label <- mmpTryLabeled "$a"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		return (ctx, Database [] [(label, mapSymbols ctx ss, Axiom [])])

mmpTheorem :: Context -> Parser (Context, Database)
mmpTheorem ctx = do
		label <- mmpTryLabeled "$p"
		mmpSeparator
		ss <- mmpIdentifiersThen "$="
		mmpSeparator
		ps <- mmpIdentifiersThen "$."
		return (ctx, Database [] [(label, mapSymbols ctx ss, Theorem [] ps)])

mmpBlock :: Context -> Parser (Context, Database)
mmpBlock ctx = do
		mmpTryUnlabeled "${"
		mmpSeparator
		(ctx2, db2) <- mmpStatements ctx
		string "$}"
		let Database inactiveStatements activeStatements = db2
		let (nonAssertions, assertions) = statSplit activeStatements
		return (ctx2, Database (inactiveStatements++nonAssertions) assertions)
	where
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
