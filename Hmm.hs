module Hmm
	(Database(Database)
	,dbConstants
	,mmParseFromString
	)

where

import Text.ParserCombinators.Parsec
import Data.List(sort)
import Data.Char(isSpace,isAscii,isControl)


data Database = Database {dbConstants::[String]} deriving Show

instance Eq Database where
	d1 == d2 = sort (dbConstants d1) == sort (dbConstants d2)



mmParseFromString :: String -> Database
mmParseFromString s =
	case parse mmParser "<string>" s of
		Left err -> error $ show err
		Right db -> db


mmParser :: Parser Database
mmParser = do
		try mmSep <|> return ()
		cs <- mmConstants `sepBy` mmSep
		try mmSep <|> return ()
		eof
		return $ Database {dbConstants = concat cs}

mmSep :: Parser ()
mmSep = do
		many1 ((space >> return ()) <|> mmComment)
		return ()
	<?> "whitespace or comment"

mmComment :: Parser ()
mmComment = do
		try (string "$(")
		manyTill anyChar (try (space >> string "$)"))
		return ()
	    <?> "comment"

mmConstants :: Parser [String]
mmConstants = do
		try (string "$c")
		mmSep
		cs <- manyTill (do {c<-mmIdentifier; mmSep; return c}) (try (string "$."))
		return cs

mmIdentifier = many1 (satisfy isMmNonSpace)

isMmNonSpace c = isAscii c && not (isSpace c) && not (isControl c)

