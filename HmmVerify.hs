module Main where

import Hmm(mmParseFromString, mmVerifiesDatabase)

main :: IO ()
main = do
	dbText <- getContents
	let (_, db) = mmParseFromString dbText
	if mmVerifiesDatabase db
		then print "Yes!" 
		else print "No..."


