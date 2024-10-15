
gzip :: [[a]] -> [[a]]
gzip xs = if not (any null xs)
    then map head xs : gzip (map tail xs)
    else []

store_two_greatest v (x,y) | v > x = (v, y)
store_two_greatest v (x,y) | v <= x && v > y = (x, v)
store_two_greatest v (x,y) = (x, y)

two_greatest (x:y:xs) = foldr store_two_greatest (if x > y then (x,y) else (y,x)) xs

sum_two_greatest xs = [let (x,y) = two_greatest v in x + y | v <- gzip xs]