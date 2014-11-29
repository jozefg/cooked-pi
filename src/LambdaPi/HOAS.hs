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
           | C String
           | IGen Int

data CExpr = Lam (CExpr -> CExpr)
           | CI IExpr

type NFI = IExpr
type NFC = CExpr

inf :: IExpr -> NFI
inf ETrue = ETrue
inf EFalse = EFalse
inf Bool = Bool
inf Star = Star
inf (C s) = C s
inf (IGen i) = IGen i
inf (Annot l r) = Annot (cnf l) (cnf r)
inf (Pi t f) = Pi (cnf t) (cnf . f)
inf (App l r) = case inf l of
  (Annot (Lam f) (CI (Pi _ g))) -> Annot (cnf . f $ CI l) (cnf . g $ CI l)
  l' -> App l' (cnf r)

cnf :: CExpr -> NFC
cnf (CI e) = CI (inf e)
cnf (Lam f) = Lam (cnf . f)

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
ieqTerm (Annot l r) (Annot l' r') = (&&) <$> ceqTerm l l' <*> ceqTerm r r'
ieqTerm (App l r) (App l' r') = (&&) <$> ieqTerm l l' <*> ceqTerm r r'
ieqTerm (Pi t f) (Pi t' g) =
  (&&) <$> ceqTerm t t' <*> (gen >>= \v -> ceqTerm (f $ CI v) (g $ CI v))
ieqTerm (IGen i) (IGen j) = return (i == j)
ieqTerm _ _ = return False
