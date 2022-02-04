
import Control.Monad (when, unless)

zipSecond :: [a] -> (a -> b) -> [(a, b)]
zipSecond (a : xs) f = (a, f a) : zipSecond xs f
zipSecond [] _ = []

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= flip when action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = mb >>= flip unless action