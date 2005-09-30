module Hmm
	(Database
	,mmLabels
	,mmParseFromString
	)

where

import Text.ParserCombinators.Parsec


data Database = Database

mmLabels :: Database -> [String]
mmLabels (Database) = []

mmParseFromString :: String -> Database
mmParseFromString s =
	case parse mmParser "<string>" s of
		Left err -> error $ show err
		Right db -> db


mmParser :: Parser Database
mmParser = do
		mmSep
		eof
		return $ Database

mmSep :: Parser ()
mmSep = do
		many ((space >> return ()) <|> mmComment)
		return ()
	<?> "whitespace or comment"

mmComment :: Parser ()
mmComment = do
		string "$("
		manyTill anyChar (try (space >> string "$)"))
		return ()
	    <?> "comment"
