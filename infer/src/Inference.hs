{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

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

data TypeError
    = TypeError String
    | RetardError String
    deriving (Show, Eq, Ord)

type Inference a = Either TypeError a

synthesizeType :: Context -> Expression -> Inference Type
synthesizeType c e = case e of
    EUnit -> pure TUnit
    EVariable v -> case lookup v c of
        Just t -> pure t
        Nothing -> throwError (TypeError "Variable type is unknown")
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
    EFunction _ _ -> throwError (RetardError "function expressions must be checked")

checkType :: Context -> Expression -> Type -> Inference ()
checkType c e t = case e of
    EFunction v e2 -> case t of
        t1 :-> t2 -> checkType ((v, t1) : c) e2 t2
        _ -> throwError (TypeError "a function found")
    EUnit -> unless (t == TUnit) (throwError (TypeError "a unit found"))
    EVariable v -> case lookup v c of
        Just t1 -> unless (t == t1) (throwError (TypeError "type error"))
        Nothing -> throwError (TypeError "unbound variable")
    EAnnotation _ _ -> throwError (RetardError "annotation types must be synthesized")
    EApplication _ _ -> throwError (RetardError "application types must be synthesized")

λ = EFunction
rv = EVariable . Variable
nv = Variable
u = EUnit
(?) = EAnnotation

-- >>> checkType [] (λ (nv "x") (rv "x")) (TUnit :-> TUnit)
-- Right ()
