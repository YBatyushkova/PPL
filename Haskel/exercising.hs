initials :: String -> String -> String
initials firstname lastname =
    [f] ++ " " ++ [l]
    where
        (f : _) = firstname
        (l : _) = lastname

greet :: String -> String
-- greet :: [Char] -> [Char]
greet name = "Hello " ++ name

ifExample x = [1, if x > 2 then 900 else 0]

caseExample lst = case lst of
    [] -> error "Empty list"
    (x:xs) -> x

head' [] = error "Empty list"
head' (x:_) = x

data Point a = Point a a

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show)

-- instance (Eq a) => Eq (Tree a) where
--     Leaf a == Leaf b = a == b
--     (Branch l1 r1) == (Branch l2 r2) = (l1 == l2) && (r1 == r2)
--     _ == _ = False

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch x y) = Branch (fmap f x) (fmap f y)

instance Foldable Tree where
    foldr f i Empty = i
    foldr f i (Leaf x) = f x i
    foldr f i (Branch l r) = foldr f (foldr f i r) l

-- instance Applicative Tree where
--     pure x = Leaf x
--     _ <*> Empty = Empty
--     Empty <*> _ = Empty
--     Leaf f <*> Leaf x = Leaf (f x)
--     Leaf f <*> Branch x y = Branch (Leaf f <*> x) (Leaf f <*> y)
--     Branch f g <*> Leaf x = Branch (f <*> Leaf x) (g <*> Leaf x) 
--     Branch f g <*> Branch x y = Branch (f <*> x) (g <*> y)

-- Tree concatenation
tconc Empty t = t
tconc t Empty = t
tconc t1 t2 = Branch t1 t2

tconcat t = foldr tconc Empty t
tconcatmap f t = tconcat (fmap f t)

-- instance Applicative Tree where
--     pure x = Leaf x
--     fs <*> xs = tconcatmap (\f -> fmap f xs) fs

instance Applicative Tree where
    pure x = Leaf x
    fs <*> xs = foldr tconc Empty (fmap (\f -> fmap f xs) fs)



--2023.07
data D2L a = Nth | Level1 a (D2L a) | Level2 [a] (D2L a) deriving (Eq, Show)
flatten Nth = []
flatten (Level1 x xs) = x : flatten xs
flatten (Level2 xs ys) = xs ++ flatten ys

instance Functor D2L where
    fmap f Nth = Nth
    fmap f (Level1 x xs) = Level1 (f x) (fmap f xs)
    fmap f (Level2 xs ys) = Level2 (fmap f xs) (fmap f ys)

instance Foldable D2L where
    foldr f i Nth = i
    foldr f i (Level1 x xs) = f x (foldr f i xs)
    foldr f i (Level2 xs ys) = foldr f (foldr f i ys) xs

Nth +++ t = t
t +++ Nth = t
(Level1 x xs) +++ t = Level1 x (xs +++ t)
(Level2 xs ys) +++ t = Level2 xs (ys +++ t)

instance Applicative D2L where
    pure x = Level1 x Nth
    fs <*> xs = foldr (+++) Nth (fmap (\f -> fmap f xs) fs)


--2023.06
data Part a = Part [a] a [a] deriving (Eq, Show)

checkpart :: Ord a => Part a -> Bool
checkpart (Part l p r) = null (filter (> p) l) && null (filter (<= p) r)

part2list :: Part a -> [a]
part2list (Part l p r) = l ++ [p] ++ r

-- list2part :: (Ord a) => [a] -> a -> Part a
-- list2part lst p = Part (filter (<= p) lst) p (filter (> p) lst)

--alternative
list2part :: (Ord a) => [a] -> a -> Part a
list2part lst p = Part lessOrEq p greater 
    where
        lessOrEq = [x | x <- lst, x <= p]
        greater = [x | x <- lst, x > p]

instance Foldable Part where
    foldr f i p = foldr f i (part2list p)


--2023.02
data BBtree a = Empty | Node a a (BBtree a) (BBtree a) deriving (Eq, Show)

bb2list :: BBtree a -> [a]
bb2list Empty = []
bb2list (Node x y t1 t2) = [x] ++ [y] ++ (bb2list t1) ++ (bb2list t2)

instance Foldable BBtree where
    foldr f i Empty = i
    foldr f i tree = foldr f i (bb2list tree)

instance Functor BBtree where
    fmap _ Empty = Empty
    fmap f (Node x y t1 t2) = Node (f x) (f y) (fmap f t1) (fmap f t2)

instance Applicative BBtree where
    pure x = Node x x Empty Empty
    _ <*> Empty = Empty
    Empty <*> _ = Empty
    (Node f g ft gt) <*> (Node x y t1 t2) = Node (f x) (g y) (ft <*> t1) (gt <*> t2)

bbmax :: (Ord a) => BBtree a -> Maybe a
bbmax Empty = Nothing
-- bbmax tree = maximum (bb2list tree)
-- already provided by the FOLDABLE
bbmax tree = Just $ maximum tree


--2021.07
gzip :: [[a]] -> [[a]]
gzip xs = if not (any null xs) 
    then (map head xs) : gzip (map tail xs)
    else []

storeTwoGreatest :: Ord a => a -> (a, a) -> (a, a)
storeTwoGreatest v (x,y) | v > x = (v,y)
storeTwoGreatest v (x,y) |x >= v && v > y = (x,v)
storeTwoGreatest v (x,y) = (x,y)

twoGreatest (x:y:xs) = foldr 
    storeTwoGreatest
    (if x > y
        then (x,y)
        else (y,x))
    xs

sunTwoGreatest xs = [let (x, y) = twoGreatest lst in x + y | lst <- gzip xs]


--2021.06
data BTT a = Nil | BinaryN a (BTT a) (BTT a) | TernaryN a (BTT a) (BTT a) (BTT a) deriving (Eq, Show)

instance Functor BTT where
    fmap _ Nil = Nil
    fmap f (BinaryN x t1 t2) = BinaryN (f x) (fmap f t1) (fmap f t1)
    fmap f (TernaryN x t1 t2 t3) = TernaryN (f x) (fmap f t1) (fmap f t1) (fmap f t3)

instance Foldable BTT where
    foldr f i Nil = i
    foldr f i (BinaryN x t1 t2) = foldr f (foldr f i t2) t1
    foldr f i (TernaryN x t1 t2 t3) = foldr f (foldr f (foldr f i t3) t2) t1

Nil +++ t = t
t +++ Nil = t
(BinaryN x t1 t2) +++ t = TernaryN x t1 t2 t
t +++ (BinaryN x t1 t2) = TernaryN x t t1 t2
(TernaryN x t1 t2 t3) +++ v@(TernaryN y l1 l2 l3) = TernaryN x t1 t2 (t3 +++ v)

instance Applicative BTT where
    pure x = BinaryN x Nil Nil
    fs <*> xs = foldr (+++) Nil (fmap (\f -> fmap f xs) fs)

--2023.01
data Tape a = Tape [a] a [a] a

-- in solution they do not reverse
instance (Eq a) => Eq (Tape a) where
    (Tape l1 x r1 b1) == (Tape l2 y r2 b2) = (reverse l1) ++ [x] ++ r1 == (reverse l2) ++ [y] ++ r2

instance (Show a) => Show (Tape a) where
    show (Tape l x r b) = show (reverse l) ++ show x ++ show r

left (Tape l x r b) = Tape (tail l) (head l) (x:r) b
right (Tape l x r b) = Tape (x:l) (head r) (tail r) b

instance Functor Tape where
    fmap f (Tape l x r b) = Tape (fmap f l) (f x) (fmap f r) (f b)

instance Applicative Tape where
    pure x = Tape [] x [] x
    (Tape fl fx fr fb) <*> (Tape l x r b) = Tape (fl <*> l) (fx x) (fr <*> r) (fb b)


--2022.06
data FPair b a = Pair a a | FPair a a b

instance (Show a, Show b) => Show (FPair b a) where
    --show (Tape l x r b) = show (reverse l) ++ show x ++ show r
    show (Pair x y) = "[" ++ show x ++ "," ++ show y ++ "]"
    show (FPair x y z) = "[" ++ show x ++ show z ++ show y ++ "]"

instance (Eq a, Eq b) => Eq (FPair b a) where
    (Pair x1 y1) == (Pair x2 y2) = x1 == x2 && y1 == y2
    (FPair x1 y1 _) == (FPair x2 y2 _) = x1 == x2 && y1 == y2

instance Functor (FPair b) where
    fmap f (Pair x y) = Pair (f x) (f y)
    fmap f (FPair x y z) = FPair (f x) (f y) z

instance Foldable (FPair b) where
    foldr f i (Pair x y) = f x (f y i)
    foldr f i (FPair x y _) = f x (f y i)

instance Applicative (FPair b) where
    pure x = Pair x x
    (FPair x1 y1 _) <*> (FPair x2 y2 z2) = FPair (x1 x2) (y1 y2) z2
    (Pair x1 y1) <*> (FPair x2 y2 z2) = FPair (x1 x2) (y1 y2) z2
    (FPair x1 y1 z1) <*> (Pair x2 y2) = FPair (x1 x2) (y1 y2) z1
    (Pair x1 y1) <*> (Pair x2 y2) = Pair (x1 x2) (y1 y2)

--2024.01
data Expr a = Var a | Const Int | Op (Expr a) (Expr a) deriving (Eq, Show)

instance Functor Expr where
    fmap f (Var x) = Var (f x)
    fmap _ (Const x) = Const x
    fmap f (Op x y) = Op (fmap f x) (fmap f y)

instance Applicative Expr where
    pure x = Var x
    _ <*> (Const x) = Const x
    (Const x) <*> _ = Const x
    (Var f) <*> (Var x) = Var (f x)
    (Var f) <*> (Op x y) = Op (fmap f x) (fmap f y)
    (Op f g) <*> x = Op (f <*> x) (g <*> x) -- ! X !

instance Monad Expr where
    return = pure
    (Const x) >>= _ = Const x
    (Var x) >>= f = f x
    (Op x y) >>= f = Op (x >>= f) (y >>= f) 

-- Var 3 >>= \x -> Var (x*2)
-- result: Var 6

-- Op (Var 1) (Var 2) >>= \x -> Var (x + 1)
-- result: Op (Var 2) (Var 3)

--2024.02 
data F b a = F (b -> b) a | Null

instance Functor (F b) where
    fmap _ Null = Null
    fmap g (F st x) = F st (g x)

instance Applicative (F b) where
    pure x = F (\st -> st) x
    -- pure = F id
    Null <*> _ = Null
    _ <*> Null = Null
    (F f x) <*> (F g y) = F (f . g) (x y)

instance Monad (F b) where
    return = pure
    Null >>= _ = Null
    (F st x) >>= f = case (f x) of
        Null -> Null
        F st' x' -> F (st . st') x'




