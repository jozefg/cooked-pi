{-# LANGUAGE LambdaCase #-}
module LambdaPi.Easy where
import           Control.Applicative hiding (Const)
import           Control.Monad       (guard)
import           Control.Monad.Gen
import           Control.Monad.Trans
import qualified Data.Map            as M

data IExpr = Var Int
           | App IExpr CExpr
           | Annot CExpr CExpr
           | ETrue
           | EFalse
           | Bool
           | Star
           | Pi CExpr CExpr
           | Const String
           | Free Int
           deriving(Eq, Show)

data CExpr = Lam CExpr
           | CI IExpr
           deriving(Eq, Show)

data VConst = CAp VConst Val
            | CVar String
            | CFree Int

data Val = VStar
         | VBool
         | VTrue
         | VFalse
         | VConst VConst
         | VPi Val (Val -> Val)
         | VLam (Val -> Val)
         | VGen Int

-- *Whistle and fidget with hands*
instance Enum Val where
  toEnum = VGen
  fromEnum _ = error "You're a bad person."

eqTerm :: Val -> Val -> Bool
eqTerm l r = runGen $ go l r
  where go VStar VStar = return True
        go VBool VBool = return True
        go VTrue VTrue = return True
        go VFalse VFalse = return True
        go (VLam f) (VLam g) = gen >>= \v -> go (f v) (g v)
        go (VPi t f) (VPi t' g) =
          (&&) <$> go t t' <*> (gen >>= \v -> go (f v) (g v))
        go (VConst c) (VConst c') = case (c, c') of
          (CVar v, CVar v') -> return (v == v')
          (CAp f a, CAp f' a') ->
            (&&) <$> go (VConst f) (VConst f') <*> go a a'
          _ -> return False
        go (VGen i) (VGen j) = return (i == j)
        go _ _ = return False

inf :: [Val] -> IExpr -> Val
inf _ ETrue = VTrue
inf _ EFalse = VFalse
inf _ Bool = VBool
inf _ Star = VStar
inf _ (Free i) = VConst (CFree i)
inf _ (Const s) = VConst (CVar s)
inf env (Annot e _) = cnf env e
inf env (Var i) = env !! i
inf env (Pi l r) = VPi (cnf env l) (\v -> cnf (v : env) r)
inf env (App l r) =
  case inf env l of
   VLam f -> f (cnf env r)
   VConst c -> VConst . CAp c $ cnf env r
   _ -> error "Impossible: evaluated ill-typed expression"

cnf :: [Val] -> CExpr -> Val
cnf env (CI e) = inf env e
cnf env (Lam c) = VLam $ \v -> cnf (v : env) c

ibind :: Int -> IExpr -> IExpr -> IExpr
ibind i e (Var j) | i == j = e
ibind i e (App l r) = App (ibind i e l) (cbind i e r)
ibind i e (Annot l r) = Annot (cbind i e l) (cbind i e r)
ibind i e (Pi l r) = Pi (cbind i e l) (cbind i e r)
ibind _ _ e  = e -- Non recursive cases

cbind :: Int -> IExpr -> CExpr -> CExpr
cbind i e (Lam b) = Lam (cbind (i + 1) e b)
cbind i e (CI c) = CI (ibind i e c)

data Env = Env { localVar :: M.Map Int Val
               , constant :: M.Map String Val }

inferType :: Env -> IExpr -> GenT Int Maybe Val
inferType _ (Var _) = lift Nothing -- The term is open
inferType (Env _ m) (Const s) = lift $ M.lookup s m
inferType (Env m _) (Free i) = lift $ M.lookup i m
inferType _ ETrue = return VBool
inferType _ EFalse = return VBool
inferType _ Bool = return VStar
inferType _ Star = return VStar
inferType env (Annot e ty) = do
  checkType env ty VStar
  let v = cnf [] ty
  checkType env e v >> return v
inferType env (App f a) = do
  ty <- inferType env f
  case ty of
   VPi aTy body -> do
     checkType env a aTy
     return (body $ cnf [] a)
   _ -> lift Nothing
inferType env (Pi ty body) = do
  checkType env ty VStar
  i <- gen
  let v = cnf [] ty
      env' = env{localVar = M.insert i v (localVar env)}
  checkType env' (cbind 0 (Free i) body) VStar
  return VStar

checkType :: Env -> CExpr -> Val -> GenT Int Maybe ()
checkType env (CI e) v = inferType env e >>= guard . (eqTerm v)
checkType env (Lam ce) (VPi argTy body) = do
  i <- gen
  let ce' = cbind 0 (Free i) ce
      env' = env{localVar = M.insert i argTy (localVar env)}
  checkType env' ce' (body $ VConst (CFree i))
checkType _ _ _ = lift Nothing

hasType :: CExpr -> CExpr -> Bool
hasType e t =
  case runGenT (inferType (Env M.empty M.empty) $ Annot e t) of
   Just _ -> True
   Nothing -> False
