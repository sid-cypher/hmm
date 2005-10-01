module Hmm
	(Context(Context)
	,ctxEmpty,ctxWithConstants
	,Database(Database)
	,mmParseFromString
	)

where

import Text.ParserCombinators.Parsec
import Data.List(sort)
import Data.Char(isSpace,isAscii,isControl)



data Database = Database
	deriving (Eq, Show)



data Context = Context {ctxConstants::[String]}
	deriving Show

instance Eq Context where
	d1 == d2 = sort (ctxConstants d1) == sort (ctxConstants d2)

ctxEmpty = Context {ctxConstants = []}

(Context cs) `ctxWithConstants` cs2 = Context {ctxConstants = cs2++cs}






mmParseFromString :: String -> (Context, Database)
mmParseFromString s =
	case parse mmpContext "<string>" s of
		Left err -> error $ show err
		Right result -> result


mmpContext :: Parser (Context, Database)
mmpContext = do
		try mmpSeparator <|> return ()
		cs <- mmConstants `sepBy` mmpSeparator
		try mmpSeparator <|> return ()
		eof
		return $ (Context {ctxConstants = concat cs}, Database)

mmpSeparator :: Parser ()
mmpSeparator = do
		many1 ((space >> return ()) <|> mmpComment)
		return ()
	<?> "token separator"

mmpComment :: Parser ()
mmpComment = do
		try (string "$(")
		manyTill anyChar (try (space >> string "$)"))
		return ()
	    <?> "comment"

mmConstants :: Parser [String]
mmConstants = do
		try (string "$c")
		mmpSeparator
		cs <- manyTill (do {c<-mmpMathSymbol; mmpSeparator; return c}) (try (string "$."))
		return cs

mmpMathSymbol = many1 (satisfy isMathSymbolChar)

isMathSymbolChar c = isAscii c && not (isSpace c) && not (isControl c)

