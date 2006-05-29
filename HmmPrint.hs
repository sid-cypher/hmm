module Main where

import Hmm(mmParseFromString)
import HmmPrinter(mmPrintProof)

main :: IO ()
main = do
	dbText <- getContents
	case mmParseFromString dbText of
		Left err -> putStrLn ("Parse error: " ++ err)
		Right (_, db) -> putStrLn $ mmPrintProof db "a1i" --TODO: get the label from the command line
