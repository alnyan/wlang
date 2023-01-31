module Types.Subst where

import Types.Data
import Data.List (nub)

---- Substitutions
-- Defines substitution behavior
class Apply t where
    apply :: Subst -> t -> t
    -- Free type variables
    ftv :: t -> [TypeVar]

-- Substitution construction
nullSubst :: Subst
nullSubst = []

(+->) :: TypeVar -> Type -> Subst
u +-> t = [(u, t)]

-- Substitution composition
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

-- Apply implementations for different types
instance Apply Type where
    -- For TVar's, check if `u` is in the subst, then return appropriate value
    apply s (TVar u) = case lookup u s of
                        Just t -> t
                        Nothing -> TVar u
    -- For any nested types, like TParameterized, apply subst to all branches
    apply s (TParameterized t gs) = (TParameterized (apply s t) (apply s gs))
    apply s (TArray t n) = (TArray (apply s t) n)
    apply s (TPointer t) = (TPointer (apply s t))
    apply s (TFunction ts t) = (TFunction (apply s ts) (apply s t))
    -- Otherwise, just return the value
    apply _ (TConst t) = (TConst t)

    ftv (TVar u) = [u]
    ftv (TParameterized t gs) = (nub . concat) [(ftv t), (ftv gs)]
    ftv (TArray t _) = ftv t
    ftv (TPointer t) = ftv t
    ftv (TFunction ts t) = (nub . concat) [(ftv ts), (ftv t)]
    ftv (TConst _) = []

-- Auto-impl Apply for [T: Apply]
instance Apply a => Apply [a] where
    apply s = map (apply s)
    ftv = nub . concat . map ftv


