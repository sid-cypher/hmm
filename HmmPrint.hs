module Main where

import Hmm(mmParseFromString)
import HmmPrinter(mmPrintProof)
import System.Environment(getArgs)

main :: IO ()
main = do
	dbText <- getContents
	[label] <- getArgs
	case mmParseFromString dbText of
		Left err -> putStrLn ("Parse error: " ++ err)
		Right (_, db) -> putStrLn $ mmPrintProof db label
