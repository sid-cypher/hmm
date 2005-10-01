module Hmm
	(Context(Context)
	,ctxEmpty,ctxWithConstants,ctxWithVariables
	,Database(Database)
	,mmParseFromString
	)

where

import Text.ParserCombinators.Parsec
import Data.List(sort)
import Data.Char(isSpace,isAscii,isControl)



data Database = Database
	deriving (Eq, Show)



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
		(ctx, db) <- mmpStatements (ctxEmpty, Database)
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
		(ctx2, db2) <- (mmpConstants (ctx, db) <|> mmpVariables (ctx, db))
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
		cs <- manyTill (do {c<-mmpMathSymbol; mmpSeparator; return c}) (try (string "$."))
		return (ctx `ctxWithConstants` cs, db)

mmpVariables :: (Context, Database) -> Parser (Context, Database)
mmpVariables (ctx, db) = do
		try (string "$v")
		mmpSeparator
		cs <- manyTill (do {c<-mmpMathSymbol; mmpSeparator; return c}) (try (string "$."))
		return (ctx `ctxWithVariables` cs, db)

mmpMathSymbol = many1 (satisfy isMathSymbolChar) <?> "math symbol"

isMathSymbolChar c = isAscii c && not (isSpace c) && not (isControl c)

