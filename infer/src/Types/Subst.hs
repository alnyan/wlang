module Types.Subst where

import Data.List (nub, intersect)
import Types.Data
import Result

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

-- Similar to substitution composition, but checks that s1 and s2 don't conflict
merge :: Subst -> Subst -> Result TypeError Subst
merge s1 s2 = if agree then Ok $ nub (s1 ++ s2) else Err $ MergeError s1 s2
    where agree = all (\u -> apply s1 (TVar u) == apply s2 (TVar u))
                      (map fst s1 `intersect` map fst s2)

-- Apply implementations for different types
instance Apply Type where
    -- For TVar's, check if `u` is in the subst, then return appropriate value
    apply s (TVar u) = case lookup u s of
                        Just t -> t
                        Nothing -> TVar u
    -- For any nested types, like TParameterized, apply subst to all branches
    apply s (TParameterized t gs) = TParameterized (apply s t) (apply s gs)
    apply s (TArray t n) = TArray (apply s t) n
    apply s (TPointer t) = TPointer (apply s t)
    apply s (TFunction ts t) = TFunction (apply s ts) (apply s t)
    -- Otherwise, just return the value
    apply _ (TConst t) = TConst t

    ftv (TVar u) = [u]
    ftv (TParameterized t gs) = nub (ftv t ++ ftv gs)
    ftv (TArray t _) = ftv t
    ftv (TPointer t) = ftv t
    ftv (TFunction ts t) = nub (ftv ts ++ ftv t)
    ftv (TConst _) = []

-- Auto-impl Apply for [T: Apply]
instance Apply a => Apply [a] where
    apply s = map (apply s)
    ftv = nub . concatMap ftv

instance Apply Constraint where
    apply s (Implements t r) = Implements (apply s t) r
    ftv (Implements t r) = nub (ftv t ++ ftv r)

instance Apply a => Apply (Qualified a) where
    apply s (ps :=> t) = apply s ps :=> apply s t
    ftv (ps :=> t) = nub (ftv t ++ concatMap ftv ps)

applyTE :: Subst -> TaggedExpr -> TaggedExpr
applyTE s (x, y) = (apply s x, apply s y)

instance Apply TaggedExprValue where
    apply s (TEBlock xs) = TEBlock (map (applyTE s) xs)
    apply s (TELet name (t, val)) = TELet name (apply s t, apply s val)
    apply s (TECall (ct, callee) args) = TECall (ct, apply s callee) (map (applyTE s) args)
    apply _ i = i
    -- TODO
    ftv _ = undefined

applyAll :: (Apply t, Eq t) => Subst -> t -> t
applyAll = loop
    where loop s t | apply s t == t = t
                   | otherwise = loop s (apply s t)
