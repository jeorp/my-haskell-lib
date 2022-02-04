 
zip_ :: [a] -> (a -> b) -> [(a, b)]
zip_ la f = zip la (map f la)
