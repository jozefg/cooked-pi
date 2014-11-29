module LambdaPi.HOAS where
import Control.Applicative
import Control.Monad.Gen

data IExpr = Var Int
           | App IExpr CExpr
           | Annot CExpr CExpr
           | ETrue
           | EFalse
           | Bool
           | Star
           | Pi CExpr (CExpr -> CExpr)
           | Const String
           | IGen Int

data CExpr = Lam (CExpr -> CExpr)
           | CI IExpr

type NFI = IExpr
type NFC = CExpr
