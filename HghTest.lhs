> module Main where
> 
> import HghCore
>
> import System.Exit
> 
> import Test.HUnit.Base
> import Test.HUnit.Text
> 
> main :: IO ()
> main = do
>	Counts {errors=e, failures=f} <- runTestTT testCases
>	case e+f of
>		0 -> return ()
>		_ -> exitWith (ExitFailure 1)
> 
> 
> testCases :: Test
> testCases =
>	TestList
>	[




>	"simple data structure tests" ~: test
>	[Var "x" @?= Var "x"

>	,inferenceRule [] [] (Var "P") @?= inferenceRule [] [] (Var "P")
>	,inferenceRule [("P","Q")] [] (Var "P") @?= inferenceRule [("Q","P")] [] (Var "P")

>	,Hypothesis (Var "P") @?= Hypothesis (Var "P")
>	,RuleApp (inferenceRule [] [] (Var "P")) [Var "Q"] [] @?= RuleApp (inferenceRule [] [] (Var "P")) [Var "Q"] []

>	,interpretProof2 (Hypothesis (Var "P")) @?= Right ([], inferenceRule [] [(Var "P")] (Var "P"))

>	,interpretProof2
>			(RuleApp (trivial $ Var "P") []
>				[Hypothesis $ Var "Q"]
>			)
>		@?= Right ([trivial $ Var "P"], trivial $ Var "Q")

<	,interpretProof2
<			(RuleApp leibniz_equiv_rightOr [Var "F"]
<				[Hypothesis (App "<->" [Var "G", Var "H"])]
<			)
<		@?= Right ([leibniz_equiv_rightOr], leibniz_equiv_rightOr)

TODO: Check the stuff from the Appendix.

>	]
>	]

> trivial :: Expression -> InferenceRule
> trivial expr = inferenceRule [] [expr] expr

> leibniz_equiv_rightOr :: InferenceRule
> leibniz_equiv_rightOr = inferenceRule
>				[]
>				[App "<->" [Var "G", Var "H"]]
>				(App "<->"
>					[App "\\/" [Var "F", Var "G"]
>					,App "\\/" [Var "F", Var "H"]
>					])

> interpretProof2 :: Proof -> Either String ([InferenceRule], InferenceRule)
> interpretProof2 proof = case interpretProof proof of
>				Left errorMessage -> Left errorMessage
>				Right d -> Right (sourceRules d, targetRule d)

