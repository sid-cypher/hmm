module Main where

import HmmImpl
import System.Environment(getArgs)

main :: IO ()
main = do
	dbText <- getContents
	[theoremLabel] <- getArgs
	case mmParseFromString dbText of
		Left err -> putStrLn ("Parse error: " ++ err)
		Right (_, db) -> do
			let (_lab, _expr, Theorem _hyps _dvrset proof) = findStatement db theoremLabel
			putStrLn $ toMMProof $ expandProof $ fromRight $ mmComputeProofTree proof




expandProof :: ProofTree Statement -> ProofTree Statement
expandProof p@(Apply (_lab, _expr, Theorem _hyps _dvrset _proof) _subproofs) = p
expandProof p = p



-- HELPER FUNCTIONS (move to HmmImpl.hs or elsewhere?)

toMMProof :: ProofTree Statement -> String
toMMProof (Apply stat subproofs) = join (map toMMProof subproofs ++ [labelOf stat])

labelOf :: Statement -> String
labelOf (lab, _expr, _info) = lab

join :: [String] -> String
join [] = ""
join [x] = x
join (x:xs) = x ++ " " ++ join xs
