
-- 1. Define a data structure, called D2L, to store lists of possibly depth two, e.g. like [1,2,[3,4],5,[6]].
-- 2. Implement a flatten function which takes a D2L and returns a flat list containing all the stored values in it in the same order.
-- 3. Make D2L an instance of Functor, Foldable, Applicative.

data D2L a = D2LNil | D2L1 a (D2L a) | D2L2 [a] (D2L a) 

instance (Show a) => Show (D2L a) where
  show D2LNil = ""
  show (D2L2 xs rest) = show xs ++ "," ++ show rest
  show (D2L1 x rest) = show x ++ "," ++ show rest

flatten :: D2L a -> [a]
flatten D2LNil = []
flatten (D2L1 x rest) = x : flatten rest
flatten (D2L2 xs rest) = xs ++ flatten rest

d2list = D2L1 1 $ D2L1 2 $ D2L1 3 $ D2L2 [1, 3] $ D2L1 2 D2LNil

instance Functor D2L where
    fmap _ D2LNil = D2LNil
    fmap f (D2L1 x rest) = D2L1 (f x) (fmap f rest)
    fmap f (D2L2 xs rest) = D2L2 (fmap f xs) (fmap f rest)

instance Foldable D2L where
    foldr f i D2LNil = i
    foldr f i (D2L1 x rest) = f x (foldr f i rest)
    foldr f i (D2L2 xs rest) = foldr f (foldr f i rest) xs

instance Applicative D2L where
    pure x = D2L1 x D2LNil
    fs <*> xs = foldr (+++) D2LNil (fmap (\f -> fmap f xs) fs)


(+++) :: D2L a -> D2L a -> D2L a
D2LNil +++ t = t
(D2L1 x rest) +++ t = D2L1 x (rest +++ t)
(D2L2 xs rest) +++ t = D2L2 xs (rest +++ t)
