module Result where

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

class IntoResult m where
    okOr :: m t -> e -> Result e t

instance IntoResult Maybe where
    okOr (Just a) _ = Ok a
    okOr Nothing e = Err e
