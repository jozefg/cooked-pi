{-# LANGUAGE LambdaCase #-}
module LambdaPi.HOAS where
import           Control.Applicative
import           Control.Monad.Gen
import           Control.Monad.Reader
import qualified Data.Map             as M

data Expr = App Expr Expr
          | Annot Expr Expr
          | ETrue
          | EFalse
          | Bool
          | Star
          | Pi Expr (Expr -> Expr)
          | Lam (Expr -> Expr)
          | C String
          | IGen Int


type NF = Expr

nf :: Expr -> NF
nf ETrue = ETrue
nf EFalse = EFalse
nf Bool = Bool
nf Star = Star
nf (C s) = C s
nf (IGen i) = IGen i
nf (Annot l _) = nf l
nf (Pi t f) = Pi (nf t) (nf . f)
nf (Lam f) = Lam (nf . f)
nf (App l r) = case nf l of
  Lam f -> nf . f $ l
  l' -> App l' (nf r)

eqTerm ::NF ->NF -> Gen Expr Bool
eqTerm Star Star = return True
eqTerm Bool Bool = return True
eqTerm ETrue ETrue = return True
eqTerm EFalse EFalse = return True
eqTerm (Annot l r) (Annot l' r') = (&&) <$> eqTerm l l' <*> eqTerm r r'
eqTerm (App l r) (App l' r') = (&&) <$> eqTerm l l' <*> eqTerm r r'
eqTerm (Pi t f) (Pi t' g) =
  (&&) <$> eqTerm t t' <*> (gen >>= \v -> eqTerm (f v) (g v))
eqTerm (IGen i) (IGen j) = return (i == j)
eqTerm _ _ = return False

eqType ::NF ->NF -> Bool
eqType l r = runGenWith (successor s) (IGen 0) $ eqTerm l r
  where s (IGen i) = IGen (i + 1)
        s _ = error "Impossible!"

data Env = Env { localVars :: M.Map Int NF
               , constants :: M.Map String NF }
type TyM = GenT Int (ReaderT Env Maybe)

inferType :: Expr -> TyM NF
inferType (IGen i) = asks (M.lookup i . localVars) >>= maybe mzero return
inferType (C s) = asks (M.lookup s . constants) >>= maybe mzero return
inferType ETrue = return Bool
inferType EFalse = return Bool
inferType Bool = return Star
inferType Star = return Star
inferType (Pi t f) = do
  checkType t Star
  let t' = nf t
  i <- gen
  local (\e -> e{localVars = M.insert i t' $ localVars e}) $
    Star <$ checkType (f $ IGen i) Star
inferType (Lam _) = mzero
inferType (Annot l r) = do
  checkType r Star
  let r' = nf r
  r' <$ checkType l r'
inferType (App l r) = do
  inferType l >>= \case
    Pi t f -> do
      f (nf r) <$ checkType r t
    _ -> mzero

checkType :: Expr ->NF -> TyM ()
checkType (Lam f) (Pi t g) = do
  i <- gen
  let t' = nf t
      rTy = nf (g $ IGen i)
  local (\e -> e{localVars = M.insert i t' $ localVars e}) $
    checkType (f $ IGen i) rTy
checkType e t = inferType e >>= guard . eqType t
