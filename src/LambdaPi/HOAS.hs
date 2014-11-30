{-# LANGUAGE LambdaCase #-}
module LambdaPi.HOAS where
import           Control.Applicative
import           Control.Monad.Gen
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Maybe

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

eqTerm :: NF -> NF -> Bool
eqTerm l r = runGenWith (successor s) (IGen 0) $ go l r
  where s (IGen i) = IGen (i + 1)
        s _ = error "Impossible!"
        go Star Star = return True
        go Bool Bool = return True
        go ETrue ETrue = return True
        go EFalse EFalse = return True
        go (Annot l r) (Annot l' r') = (&&) <$> go l l' <*> go r r'
        go (App l r) (App l' r') = (&&) <$> go l l' <*> go r r'
        go (Pi t f) (Pi t' g) =
          (&&) <$> go t t' <*> (gen >>= \v -> go (f v) (g v))
        go (IGen i) (IGen j) = return (i == j)
        go _ _ = return False

data Env = Env { localVars :: M.Map Int NF
               , constants :: M.Map String NF }
type TyM = ReaderT Env (GenT Int Maybe)

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

checkType :: Expr -> NF -> TyM ()
checkType (Lam f) (Pi t g) = do
  i <- gen
  let t' = nf t
      rTy = nf (g $ IGen i)
  local (\e -> e{localVars = M.insert i t' $ localVars e}) $
    checkType (f $ IGen i) rTy
checkType e t = inferType e >>= guard . eqTerm t

hasType :: Expr -> Expr -> Bool
hasType e = isJust
            . runGenT
            . flip runReaderT (Env M.empty M.empty)
            . checkType e
            . nf
