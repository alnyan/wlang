module Utils where

-- Helper function to perform monadic map over pairs
mapMT :: (Traversable t, Monad m) => (a -> b -> m c) -> t (a, b) -> m (t c)
mapMT f = mapM (\(x, y) -> f x y)
