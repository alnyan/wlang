{-# LANGUAGE FlexibleInstances #-}

module Unify where

import Error
import Subst
import Type

-- Unification
class Unify t where
    mgu :: t -> t -> Result TypeError Subst

instance Unify Type where
    mgu (TFunction ts t) (TFunction ts' t') = do s1 <- mgu ts ts'
                                                 s2 <- mgu (apply s1 t) (apply s1 t')
                                                 return (s2 @@ s1)
    mgu (TParameterized t ts) (TParameterized t' ts') = do s1 <- mgu t t'
                                                           s2 <- mgu (apply s1 ts) (apply s1 ts')
                                                           return (s2 @@ s1)
    mgu (TPointer t) (TPointer t') = mgu t t'
    mgu (TArray t _) (TArray t' _) = mgu t t'
    mgu (TConst tc1) (TConst tc2) | tc1 == tc2 = Ok nullSubst
    mgu (TVar u) t = varBind u t
    mgu t (TVar u) = varBind u t
    mgu t1 t2 = Err $ UnifyError t1 t2

-- Useful for unifying lists of types
instance Unify [Type] where
    mgu as bs | length as /= length bs = Err $ ArgumentCountMismatch as bs
              | otherwise = loop nullSubst (zip as bs)
                where loop s [] = Ok s
                      loop s ((t, t'):ts) = do s' <- mgu (apply s t) (apply s t')
                                               loop (s' @@ s) ts

varBind :: TypeVar -> Type -> Result TypeError Subst
varBind (TVInt n) t | not $ isCoreInteger t = Err $ IntUnifyError (TVInt n) t
varBind (TVFloat n) t | not $ isCoreFloat t = Err $ FloatUnifyError (TVFloat n) t
varBind u t | t == TVar u       = Ok nullSubst
            | u `elem` ftv t    = Err $ OccursCheck u t
            | otherwise         = Ok (u +-> t)

