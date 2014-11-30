module Main where
import LambdaPi.Bound
import Test.Tasty
import Test.Tasty.HUnit

assertType :: String -> TestName -> (Expr Int, Expr Int) -> TestTree
assertType s n (e, t) = testCase n $ assertBool s (hasType e t)

consts :: TestTree
consts = testGroup "Constant Tests"
         [ assertType "ETrue is wrong" "True" (ETrue, Bool)
         , assertType "EFalse is wrong" "False" (EFalse, Bool)
         , assertType "Bool is wrong" "Bool" (Bool, Star) ]

boolId :: TestTree
boolId = assertType "Simple lambdas failed" "Bool identity"
         (lam 0 (Var 0), pit 0 Bool Bool)

app :: TestTree
app = assertType "Application fails" "Application"
      (App (Annot (lam 0 $ Var 0)
                  (pit 0 Bool Bool))
           ETrue
      , Bool)

main :: IO ()
main = defaultMain . testGroup "bound Tests"
       $ [consts, boolId, app]
