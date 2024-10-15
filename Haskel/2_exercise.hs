import Text.XHtml (base, table)
import System.Win32 (xBUTTON1)
-- myFunc :: Num a => a -> a -> a

-- class Equal a where
--     (==) :: a -> a -> Bool
--     x /= y = not (x == y)

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show)

-- instance (Eq a) => Eq (Tree a) where
--     Leaf a == Leaf b = a == b
--     Branch l1 r1 == Branch l2 r2 = l1 == l2 && r1 == r2
--     _ == _ = False

-- SUBCLASSES can be defined as:
-- class Eq a => Ord a where
--     (<), (<=), (>), (>=) :: a -> a -> Bool
--     min, max :: a -> a -> a

-- For custom SHOW create an instance
-- instance (Show a) => Show (Tree a) where
--     show (Leaf a) = show a
--     show (Branch l r) = "<" ++ show l ++ "|" ++ show r ++ ">"

-- FOLDABLE
-- FOLD <binary function> <accumulator> <data>
treeFoldr f z Empty = z
treeFoldr f z (Leaf x) = f x z
treeFoldr f z (Branch l r) = treeFoldr f (treeFoldr f z r) l

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr = treeFoldr

-- `foldl` can be expressed in terms of `foldr`
-- as `foldr` can work on infinite lists, while
-- `foldl` cannot.
foldl f a bs = foldr (\b g x -> g(f x b)) id bs a

-- instance Foldable Maybe where
--     foldr _ z Nothing = z
--     foldr f z (Just x) = f x z

data Result a = Err | Ok a deriving (Eq, Ord, Show)

safediv :: Int -> Int -> Result Int
safediv n m = 
    if m == 0
        then Err
        else Ok (n `div` m)

-- FUNCTOR
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Empty = Empty
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap _ Err = Err
    fmap f (Ok x) = Ok (f x)
-- we can call fmap with an infix notation as "f <$> t"

-- APPLICATIVE
instance Applicative Result where
    (<*>) :: Result (a -> b) -> Result a -> Result b
    pure x = Ok x -- wraps an argument in our structure
    _ <*> Err = Err
    Err <*> _ = Err
    (Ok f) <*> (Ok x) = Ok (f x)
    -- (Ok f) <*> x = f <$> x 

-- instance Applicative Tree where
--     (<*>) :: Tree (a -> b) -> Tree a -> Tree b
--     pure x = Leaf x
--     _ <*> Empty = Empty
--     Empty <*> _ = Empty
--     (Leaf f) <*> (Leaf x) = Leaf (f x)
--     (Leaf f) <*> (Branch l r) = Branch (Leaf f <*> l) (Leaf f <*> r)
--     (Branch f g) <*> (Leaf x) = Branch (f <*> Leaf x) (g <*> Leaf x)
--     (Branch f g) <*> (Branch l r) = Branch (f <*> l) (g <*> r)

-- Tree concatenation
tconc Empty t = t
tconc t Empty = t
tconc t1 t2 = Branch t1 t2

tconcat t = foldr tconc Empty t
tconcmap f t = tconcat (fmap f t)

-- for lists concatMap f l = concat (map f l)

instance Applicative Tree where 
    pure x = Leaf x
    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    fs <*> xs = tconcmap (\f -> fmap f xs) fs


-- instance Applicative [] where
--     pure x = [x]
--     fs <*> xs = concatMap (\f -> map f xs) fs

{--
APPLICATIVE RULES:
pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
--}