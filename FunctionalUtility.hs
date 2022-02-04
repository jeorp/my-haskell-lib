 
zip_ :: [a] -> (a -> b) -> [(a, b)]
zip_ (a : xs) f = (a, f a) : zip_ xs f
zip_ [] _ = []
