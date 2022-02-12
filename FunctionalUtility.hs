
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

safeTail :: [a] -> Maybe a
safeTail (a: as) = Just a
safeTail [] = Nothing 

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= flip when action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = mb >>= flip unless action

or_implyl :: [a] -> (a -> a -> Bool) -> (a -> Bool)
or_implyl xs f = \a -> or $ f a <$> xs 

and_implyl :: [a] -> (a -> a -> Bool) -> (a -> Bool)
and_implyl xs f = \a -> and $ f a <$> xs

or_implyr :: [a] -> (a -> a -> Bool) -> (a -> Bool)
or_implyr xs f = \a -> or $ flip f a <$> xs 

and_implyr :: [a] -> (a -> a -> Bool) -> (a -> Bool)
and_implyr xs f = \a -> and $ flip f a <$> xs

or_eq :: Eq a => [a] -> (a -> Bool)
or_eq xs = or_implyl xs (==)

and_eq :: Eq a => [a] -> (a -> Bool)
and_eq xs = and_implyl xs (==)

or_neq :: Eq a => [a] -> (a -> Bool)
or_neq xs = or_implyl xs (/=)

and_neq :: Eq a => [a] -> (a -> Bool)
and_neq xs = and_implyl xs (/=)