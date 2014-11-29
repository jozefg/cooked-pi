{-# LANGUAGE LambdaCase, DeriveFunctor #-}
module LambdaPi.Bound where
import           Bound
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Gen
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Maybe
import           Prelude.Extras

data Expr a = Var a
            | App (Expr a) (Expr a)
            | Annot (Expr a) (Expr a)
            | ETrue
            | EFalse
            | Bool
            | Star
            | Pi (Expr a) (Scope () Expr a)
            | Lam (Scope () Expr a)
            deriving(Functor, Eq)

instance Eq1 Expr where (==#) = (==)
instance Applicative Expr where
  pure = return
  (<*>) = ap
instance Monad Expr where
  return = Var
  Var a >>= f = f a
  (App l r) >>= f = App (l >>= f) (r >>= f)
  ETrue >>= _ = ETrue
  EFalse >>= _ = EFalse
  Bool >>= _ = Bool
  Star >>= _ = Star
  Annot l r >>= f = Annot (l >>= f) (r >>= f)
  Pi l s >>= f = Pi (l >>= f) (s >>>= f)
  Lam e >>= f = Lam (e >>>= f)

type Val = Expr -- Represents normalized expressions

nf :: Expr a -> Val a
nf = \case
  (Annot e t) -> Annot (nf e) (nf t)
  (Lam e) -> Lam (toScope . nf . fromScope $ e)
  (Pi l r) -> Pi (nf l) (toScope . nf . fromScope $ r)
  (App l r) ->
    case l of
     Lam f -> nf (instantiate1 r f)
     l' -> App l' (nf r)
  e -> e

type Env = M.Map Int (Val Int)
type TyM = ReaderT Env (GenT Int Maybe)

unbind :: (MonadGen a m, Functor m, Monad f) => Scope () f a -> m (a, f a)
unbind scope = ((,) <*> flip instantiate1 scope . return) <$> gen

unbindWith :: Monad f => a -> Scope () f a -> f a
unbindWith = instantiate1 . return

inferType :: Expr Int -> TyM (Val Int)
inferType (Var i) = asks (M.lookup i) >>= maybe mzero return
inferType ETrue = return Bool
inferType EFalse = return Bool
inferType Bool = return Star
inferType Star = return Star
inferType (Lam _) = mzero -- We can only check lambdas
inferType (Annot e ty) = do
  checkType ty Star
  let v = nf ty
  v <$ checkType e v
inferType (App f a) = do
  ty <- inferType f
  case ty of
   Pi aTy body -> nf (App (Lam body) a) <$ checkType a aTy
   _ -> mzero
inferType (Pi t s) = do
  checkType t Star
  (newVar, s') <- unbind s
  local (M.insert newVar $ nf t) $
    Star <$ checkType s' Star

checkType :: Expr Int -> Val Int -> TyM ()
checkType (Lam s) (Pi t ts) = do
  (newVar, s') <- unbind s
  local (M.insert newVar (nf t)) $
    checkType s' (nf $ unbindWith newVar ts)
checkType e t = inferType e >>= guard . (== t)

lam :: Eq a => a -> Expr a -> Expr a
lam a = Lam . abstract1 a

pit :: Eq a => a -> Expr a -> Expr a -> Expr a
pit v t = Pi t . abstract1 v

typecheck :: Expr Int -> Expr Int -> Bool
typecheck e = isJust
              . runGenT
              . flip runReaderT M.empty
              . checkType e
