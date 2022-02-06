
import Control.Monad (when, unless)

foldlC :: (b -> a -> b) -> (b -> Bool) -> b -> [a] -> b
foldlC cf pred b (x:xs) = 
  let cal = cf b x
  in if pred cal
    then foldlC cf pred cal xs 
    else cal 
foldlC cf pred b [] = b 

zipSecond :: [a] -> (a -> b) -> [(a, b)]
zipSecond (a : xs) f = (a, f a) : zipSecond xs f
zipSecond [] _ = []

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= flip when action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = mb >>= flip unless action