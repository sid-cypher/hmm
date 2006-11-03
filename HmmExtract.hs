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
			putStrLn $ toMMProof $ fromRight $ expandProofTree $ toProofTree proof



expandProofTree :: ProofTree Statement -> Either String (ProofTree Statement)
expandProofTree (Apply (_lab, _expr, Theorem hyps _dvrset proof) subproofs) =
	let
		hypMap = zipWith (\(lab, _, _) subproof -> (lab, subproof))
				hyps
				subproofs
	in case substituteHyps hypMap (toProofTree proof) of
		Left err -> Left err
		Right p -> expandProofTree p
expandProofTree (Apply stat subproofs) =
	case joinEithers (map expandProofTree subproofs) of
		Left err -> Left err
		Right ps -> Right $ Apply stat ps

substituteHyps :: [(Label, ProofTree Statement)] -> ProofTree Statement -> Either String (ProofTree Statement)
substituteHyps m (Apply stat subproofs) | isAssertion stat =
	case joinEithers (map (substituteHyps m) subproofs) of
		Left err -> Left err
		Right ps -> Right $ Apply stat ps
substituteHyps m proofTree@(Apply (lab, _expr, _info) []) =
	case lookup lab m of
		Just p -> Right p
		Nothing -> Left ("could not find label '" ++ lab ++ "' in map " ++ show m ++ ".  ProofTree = " ++ show proofTree)
substituteHyps _ _ = Left "this should not occur"



-- HELPER FUNCTIONS (move to HmmImpl.hs or elsewhere?)

toMMProof :: ProofTree Statement -> String
toMMProof (Apply stat subproofs) = join (map toMMProof subproofs ++ [labelOf stat])

labelOf :: Statement -> String
labelOf (lab, _expr, _info) = lab

join :: [String] -> String
join [] = ""
join [x] = x
join (x:xs) = x ++ " " ++ join xs

toProofTree :: Proof -> ProofTree Statement
toProofTree = fromRight . mmComputeProofTree

joinEithers :: [Either String a] -> Either String [a]
joinEithers [] = Right []
joinEithers e@(Left _ : _) = Left (unlines (lefts e))
joinEithers (Right a : rest) =
	case joinEithers rest of
		Left err -> Left err
		Right as -> Right (a:as)

lefts :: [Either e a] -> [e]
lefts [] = []
lefts (Left e : rest) = e : lefts rest
lefts (Right _ : rest) = lefts rest
