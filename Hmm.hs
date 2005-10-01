module Hmm
	(Context(Context)
	,ctxEmpty,ctxWithConstants,ctxWithVariables
	,Database(Database)
	,Statement
	,StatementInfo(DollarF)
	,Symbol(Var,Con)
	,mmParseFromString
	)

where

import Text.ParserCombinators.Parsec
import Data.List(sort)
import Data.Char(isSpace,isAscii,isControl)



data Database = Database [Statement]
	deriving (Eq, Show)

type Statement = (String, [Symbol], StatementInfo)

data StatementInfo = DollarF
	deriving (Eq, Show)

data Symbol = Var String | Con String
	deriving (Eq, Show)


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
		(ctx2, db2) <- (mmpConstants (ctx, db) <|> mmpVariables (ctx, db) <|> mmpDollarF (ctx, db))
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
		try (string "$c")
		mmpSeparator
		cs <- mmpUntilDollatDot
		return (ctx `ctxWithConstants` cs, db)

mmpVariables :: (Context, Database) -> Parser (Context, Database)
mmpVariables (ctx, db) = do
		try (string "$v")
		mmpSeparator
		cs <- mmpUntilDollatDot
		return (ctx `ctxWithVariables` cs, db)

mmpDollarF :: (Context, Database) -> Parser (Context, Database)
mmpDollarF (ctx, db) = do
		label <- try $ do
				label <- mmpIdentifier
				mmpSeparator
				string "$f"
				return label
		mmpSeparator
		ss <- mmpUntilDollatDot
		let symbols = map (\s ->
				if s `elem` ctxConstants ctx then Con s
				else if s `elem` ctxVariables ctx then Var s
				else error ("Unknown math symbol " ++ s)
			) ss
		return (ctx, db `dbWithStatement` (label, symbols, DollarF))

mmpUntilDollatDot :: Parser [String]
mmpUntilDollatDot = manyTill (do {s<-mmpIdentifier; mmpSeparator; return s}) (try (string "$."))

mmpIdentifier :: Parser String
mmpIdentifier = many1 (satisfy isMathSymbolChar) <?> "math symbol"

isMathSymbolChar :: Char -> Bool
isMathSymbolChar c = isAscii c && not (isSpace c) && not (isControl c)

