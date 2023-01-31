module Lib where

import Data.List (nub)

---- Errors
data TypeError = UnifyError Type Type
               | OccursCheck TypeVar Type
               | ArgumentCountMismatch [Type] [Type]
               | IntUnifyError TypeVar Type
               | FloatUnifyError TypeVar Type
    deriving Show

---- Types
-- Type identifier (generated from high-level types through internment process)
type Id = String

-- Types
-- TODO: struct/enum types
-- TODO: references
-- TODO: slices
-- TODO: anything else
data Type = TVar TypeVar                -- e.g. `{integer #n}`, `{float #m}`, `T`
          | TConst Id                   -- e.g. `MyType`
          | TParameterized Type [Type]  -- e.g. `Result<Type1, E>`
          | TArray Type Int             -- e.g. `[T; 1024]`
          | TPointer Type               -- e.g. `*T`
          | TFunction [Type] Type       -- e.g. `fn(T, U, V, ...) -> R`
    deriving (Eq, Show)

data TypeVar = TVAny Id                 -- Can take any type, like `T` in `struct X<T>`
             | TVInt Int                -- Can take any *integer* type, used for integer literals
             | TVFloat Int              -- Can take any *float* type, used for float literals
    deriving (Eq, Show)

-- Common types
tUnit = TConst "()"
tI64 = TConst "i64"
tI32 = TConst "i32"
tI16 = TConst "i16"
tI8 = TConst "i8"
tU64 = TConst "u64"
tU32 = TConst "u32"
tU16 = TConst "u16"
tU8 = TConst "u8"
tF64 = TConst "f64"
tF32 = TConst "f32"

tCoreIntNames = [ "i64", "i32", "i16", "i8",
                  "u64", "u32", "u16", "u8" ]
tCoreFloatNames = [ "f32", "f64" ]

isCoreInteger :: Type -> Bool
isCoreInteger (TConst tc) = tc `elem` tCoreIntNames
isCoreInteger _ = False

isCoreFloat :: Type -> Bool
isCoreFloat (TConst tc) = tc `elem` tCoreFloatNames
isCoreFloat _ = False

---- Substitutions
-- Defines substitution behavior
class Apply t where
    apply :: Subst -> t -> t
    -- Free type variables
    ftv :: t -> [TypeVar]

-- Substitutions
type Subst = [(TypeVar, Type)]

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


-- Unification
class Unify t where
    mgu :: t -> t -> Either TypeError Subst

instance Unify Type where
    mgu (TFunction ts t) (TFunction ts' t') = do s1 <- mgu ts ts'
                                                 s2 <- mgu (apply s1 t) (apply s1 t')
                                                 return (s2 @@ s1)
    mgu (TParameterized t ts) (TParameterized t' ts') = do s1 <- mgu t t'
                                                           s2 <- mgu (apply s1 ts) (apply s1 ts')
                                                           return (s2 @@ s1)
    mgu (TPointer t) (TPointer t') = mgu t t'
    mgu (TConst tc1) (TConst tc2) | tc1 == tc2 = Right nullSubst
    mgu (TVar u) t = varBind u t
    mgu t (TVar u) = varBind u t
    mgu t1 t2 = Left $ UnifyError t1 t2

-- Useful for unifying lists of types
instance Unify [Type] where
    mgu as bs | length as /= length bs = Left $ ArgumentCountMismatch as bs
              | otherwise = loop nullSubst (zip as bs)
                where loop s [] = Right s
                      loop s ((t, t'):ts) = do s' <- mgu (apply s t) (apply s t')
                                               loop (s' @@ s) ts

varBind :: TypeVar -> Type -> Either TypeError Subst
varBind (TVInt n) t | not $ isCoreInteger t = Left $ IntUnifyError (TVInt n) t
varBind (TVFloat n) t | not $ isCoreFloat t = Left $ FloatUnifyError (TVFloat n) t
varBind u t | t == TVar u       = Right nullSubst
            | u `elem` ftv t    = Left $ OccursCheck u t
            | otherwise         = Right (u +-> t)
