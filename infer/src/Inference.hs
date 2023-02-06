{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- That whole module is just an attempt at imlementing a simple bidirectional type checking
-- It would have to be updated to reuse all the ast/type information from the rest of the project
-- Also, currently there's no subsumption/∀ mechanism, but it would be needed for traits
module Inference where

import Control.Monad.Except

newtype Variable = Variable String
    deriving (Show, Eq, Ord)

newtype TVariable = TypeVariable String
    deriving (Show, Eq, Ord)

data Type where
    TUnit :: Type
    (:->) :: Type -> Type -> Type
deriving instance Show Type
deriving instance Ord Type
deriving instance Eq Type

data Expression
    = EVariable Variable
    | EUnit
    | EFunction Variable Expression
    | EApplication Expression Expression
    | EAnnotation Expression Type
    deriving (Show, Eq, Ord)

type Context = [(Variable, Type)]

newtype TypeError = TypeError String
    deriving (Show, Eq, Ord)

type Inference a = Either TypeError a

-- There are introductions, eliminations and stuff like variables.
-- The principal judgement of introductions is usually the checking.
-- The principal judgment of eliminations is always the synthesis.

synthesizeType :: Context -> Expression -> Inference Type
synthesizeType c e = case e of
    EVariable v -> case lookup v c of
        Just t -> pure t
        Nothing -> throwError (TypeError $ "unbound variable: " <> show v)
    EAnnotation e2 t -> do
        checkType c e2 t
        pure t
    EApplication e2 e3 -> do
        t <- synthesizeType c e2
        (t1, t2) <- case t of
            t1 :-> t2 -> pure (t1, t2)
            _ -> throwError (TypeError "not a function")
        checkType c e3 t1
        pure t2

checkType :: Context -> Expression -> Type -> Inference ()
checkType c e t = case e of
    EFunction v e2 -> case t of
        t1 :-> t2 -> checkType ((v, t1) : c) e2 t2
        _ -> typingError e ("a function found, but " <> show t <> " expected")
    EUnit -> unless (t == TUnit) (typingError e ("a unit found, but " <> show t <> " expected"))
    EVariable v -> case lookup v c of
        Just t1 -> unless (t == t1) (typingError e ("a " <> show t1 <> " found, but " <> show t <> " expected"))
        Nothing -> throwError (TypeError $ "unbound variable: " <> show v)
    _ -> do
        t1 <- synthesizeType c e
        unless
            (t1 <: t)
            (typingError e ("subsumption failed; expected " <> show t <> "; got " <> show t1))

typingError :: (MonadError TypeError m, Show a) => a -> String -> m ()
typingError e msg = throwError (TypeError $ "error in: " <> show e <> "; " <> msg)

-- | Subsumption check.
(<:) :: Type -> Type -> Bool
t1 <: t2 = t1 == t2

λ = EFunction
rv = EVariable . Variable
nv = Variable
u = EUnit
(?) = EAnnotation

-- >>> synthesizeType [] ((λ (nv "x") (λ (nv "y") (rv "y"))) ? (TUnit :-> TUnit))
-- Left (TypeError "error in: EFunction (Variable \"y\") (EVariable (Variable \"y\")); a function found, but TUnit expected")
