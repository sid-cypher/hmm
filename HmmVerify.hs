module Main where

import Hmm(mmParseFromString, mmVerifiesAll)

main :: IO ()
main = do
	dbText <- getContents
	let (_, db) = mmParseFromString dbText
	sequence_ $
		map (\(lab, correct) -> putStrLn (lab ++ ": " ++ if correct then "verified" else "INCORRECT!")) (mmVerifiesAll db)
