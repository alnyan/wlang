module Utils where

-- Helper function to perform monadic map over pairs
mapMT :: (Traversable t, Monad m) => (a -> b -> m c) -> t (a, b) -> m (t c)
mapMT f = mapM (uncurry f)

mapPair :: (a -> b, c -> d) -> (a, c) -> (b, d)
mapPair (f, g) (x, y) = (f x, g y)
