module Hmm
	(Database(Database)
	,dbEmpty,dbWithConstants
	,mmParseFromString
	)

where

import Text.ParserCombinators.Parsec
import Data.List(sort)
import Data.Char(isSpace,isAscii,isControl)




data Database = Database {dbConstants::[String]} deriving Show

dbEmpty = Database {dbConstants = []}

(Database cs) `dbWithConstants` cs2 = Database {dbConstants = cs2++cs}

instance Eq Database where
	d1 == d2 = sort (dbConstants d1) == sort (dbConstants d2)






mmParseFromString :: String -> Database
mmParseFromString s =
	case parse mmpDatabase "<string>" s of
		Left err -> error $ show err
		Right db -> db


mmpDatabase :: Parser Database
mmpDatabase = do
		try mmpSeparator <|> return ()
		cs <- mmConstants `sepBy` mmpSeparator
		try mmpSeparator <|> return ()
		eof
		return $ Database {dbConstants = concat cs}

mmpSeparator :: Parser ()
mmpSeparator = do
		many1 ((space >> return ()) <|> mmpComment)
		return ()
	<?> "whitespace or comment"

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

