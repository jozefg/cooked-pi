module Main where
import LambdaPi.Easy
import Test.Tasty
import Test.Tasty.HUnit

assertType :: String -> TestName -> (CExpr, CExpr) -> TestTree
assertType s n (e, t) = testCase n $ assertBool s (hasType e t)

constants :: TestTree
constants = testGroup "Constant Tests"
            [ assertType "ETrue is wrong" "True" (CI ETrue, CI Bool)
            , assertType "EFalse is wrong" "False" (CI EFalse, CI Bool)
            , assertType "Bool is wrong" "Bool" (CI Bool, CI Star) ]

boolId :: TestTree
boolId = assertType "Simple lambdas failed" "Bool identity"
         (Lam (CI $ Var 0), CI $ Pi (CI Bool) (CI Bool))

app :: TestTree
app = assertType "Application fails" "Application"
      (CI $
       App (Annot (Lam . CI $ Var 0)
            (CI $ Pi (CI Bool) (CI Bool)))
           (CI ETrue)
      , CI Bool)

main :: IO ()
main = defaultMain . testGroup "simple-easy Tests"
       $ [constants, boolId, app]
