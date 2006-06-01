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
> 
>	"simple data structure tests" ~: test
>	[Var "x" @?= Var "x"
>	,inferenceRule [] [] (Var "P") @?= inferenceRule [] [] (Var "P")
>	,inferenceRule [("P","Q")] [] (Var "P") @?= inferenceRule [("Q","P")] [] (Var "P")
>	,Hypothesis (Var "P") @?= Hypothesis (Var "P")
>	,RuleApp (inferenceRule [] [] (Var "P")) [Var "Q"] [] @?= RuleApp (inferenceRule [] [] (Var "P")) [Var "Q"] []
>	]
>	]
