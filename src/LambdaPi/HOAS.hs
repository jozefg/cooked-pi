module LambdaPi.HOAS where
import Control.Applicative
import Control.Monad.Gen

data IExpr = App IExpr CExpr
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

-- *Whistle and fidget with hands*
instance Enum IExpr where
  toEnum = IGen
  fromEnum _ = error "You're a bad person."

ceqTerm :: NFC -> NFC -> Gen NFI Bool
ceqTerm (Lam f) (Lam g) = gen >>= \v -> ceqTerm (f $ CI v) (g $ CI v)
ceqTerm (CI l) (CI r) = ieqTerm l r
ceqTerm _ _ = return False

ieqTerm :: NFI -> NFI -> Gen IExpr Bool
ieqTerm Star Star = return True
ieqTerm Bool Bool = return True
ieqTerm ETrue ETrue = return True
ieqTerm EFalse EFalse = return True
ieqTerm (Pi t f) (Pi t' g) =
  (&&) <$> ceqTerm t t' <*> (gen >>= \v -> ceqTerm (f $ CI v) (g $ CI v))
ieqTerm (IGen i) (IGen j) = return (i == j)
ieqTerm _ _ = return False
