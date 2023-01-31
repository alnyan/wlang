module Error where

import Type (Type(..), TypeVar(..))

data Result e t = Ok t | Err e
    deriving (Show, Eq)

instance Applicative (Result e) where
    pure = Ok
    Err e <*> _ = Err e
    Ok f <*> t = fmap f t

instance Functor (Result e) where
    fmap _ (Err e) = Err e
    fmap f (Ok t) = Ok (f t)

instance Monad (Result e) where
    return = pure
    Err e >>= _ = Err e
    Ok t >>= k = k t

---- Errors enum
data TypeError = UnifyError Type Type
               | OccursCheck TypeVar Type
               | ArgumentCountMismatch [Type] [Type]
               | IntUnifyError TypeVar Type
               | FloatUnifyError TypeVar Type
    deriving (Show, Eq)
