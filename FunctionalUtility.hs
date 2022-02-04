
import Control.Monad (when, unless)

zip_ :: [a] -> (a -> b) -> [(a, b)]
zip_ (a : xs) f = (a, f a) : zip_ xs f
zip_ [] _ = []

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= flip when action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = mb >>= flip unless action