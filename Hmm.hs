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



data Database = Database [Statement]
	deriving Show

instance Eq Database where
	Database ss1 == Database ss2 = sort ss1 == sort ss2

type Statement = (String, [Symbol], StatementInfo)

data StatementInfo = DollarE | DollarF | Axiom | Theorem [String]
	deriving (Eq, Show, Ord)

data Symbol = Var String | Con String
	deriving (Eq, Show, Ord)


(Database ss) `dbWithStatement` s = Database (s:ss)



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
		(ctx, db) <- mmpStatements (ctxEmpty, Database [])
		eof
		return (ctx, db)

mmpStatements :: (Context, Database) -> Parser (Context, Database)
mmpStatements (ctx, db) =
		do
			(ctx2, db2) <- mmpStatement (ctx, db)
			(do
				mmpSeparator
				mmpStatements (ctx2, db2)
			 <|> return (ctx2, db2))
		<|> return (ctx, db)

mmpStatement :: (Context, Database) -> Parser (Context, Database)
mmpStatement (ctx, db) = do
		(ctx2, db2) <- (mmpConstants (ctx, db) <|> mmpVariables (ctx, db) <|> mmpDollarE (ctx, db) <|> mmpDollarF (ctx, db) <|> mmpAxiom (ctx, db) <|> mmpTheorem (ctx, db))
		return (ctx2, db2)

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
		return (ctx, db `dbWithStatement` (label, mapSymbols ctx ss, DollarE))

mmpDollarF :: (Context, Database) -> Parser (Context, Database)
mmpDollarF (ctx, db) = do
		label <- mmpTryLabeled "$f"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		return (ctx, db `dbWithStatement` (label, mapSymbols ctx ss, DollarF))

mmpAxiom :: (Context, Database) -> Parser (Context, Database)
mmpAxiom (ctx, db) = do
		label <- mmpTryLabeled "$a"
		mmpSeparator
		ss <- mmpIdentifiersThen "$."
		return (ctx, db `dbWithStatement` (label, mapSymbols ctx ss, Axiom))

mmpTheorem :: (Context, Database) -> Parser (Context, Database)
mmpTheorem (ctx, db) = do
		label <- mmpTryLabeled "$p"
		mmpSeparator
		ss <- mmpIdentifiersThen "$="
		mmpSeparator
		ps <- mmpIdentifiersThen "$."
		return (ctx, db `dbWithStatement` (label, mapSymbols ctx ss, Theorem ps))

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
