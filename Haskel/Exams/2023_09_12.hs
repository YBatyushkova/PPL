{--
Consider the binary tree data structure as seen in class.
1. Define a function btrees which takes a value x and returns an infinite list of binary trees, where:
  1. all the leaves contain x,
  2. each tree is complete,
  3. the first tree is a single leaf, and each tree has one level more than its previous one in the list.
2. Define an infinite list of binary trees, which is like the previous one, but the first leaf contains the integer 1, and each subsequent tree contains leaves that have the value of the previous one incremented by one.
E.g. [Leaf 1, (Branch (Leaf 2)(Leaf 2), ...]
3. Define an infinite list containing the count of nodes of the trees in the infinite list of the previous point. E.g. [1, 3, ...]
Write the signatures of all the functions you define.
--}

data Tree x = Leaf x | Branch (Tree x) (Tree x) deriving (Eq, Show)

btrees :: a -> [Tree a]
btrees x = Leaf x : [Branch t t | t <- btrees x]

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

btrees' :: [Tree Int]
btrees' = Leaf 1 : [Branch (fmap (+ 1) t) (fmap (+ 1) t) | t <- btrees']

count = [2^x - 1 | x <- [1 ..]]

