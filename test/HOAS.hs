module Main where
import LambdaPi.HOAS
import Test.Tasty
import Test.Tasty.HUnit

assertType :: String -> TestName -> (Expr, Expr) -> TestTree
assertType s n (e, t) = testCase n $ assertBool s (hasType e t)

consts :: TestTree
consts = testGroup "Constant Tests"
         [ assertType "ETrue is wrong" "True" (ETrue, Bool)
         , assertType "EFalse is wrong" "False" (EFalse, Bool)
         , assertType "Bool is wrong" "Bool" (Bool, Star) ]

boolId :: TestTree
boolId = assertType "Simple lambdas failed" "Bool identity"
         (Lam id, Pi Bool $ const Bool)

app :: TestTree
app = assertType "Application fails" "Application"
      (App (Annot (Lam id) (Pi Bool $ const Bool))
           ETrue
      , Bool)

main :: IO ()
main = defaultMain . testGroup "HOAS Tests"
       $ [consts, boolId, app]
