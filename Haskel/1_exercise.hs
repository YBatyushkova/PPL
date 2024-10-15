import GHC.Generics (Meta (MetaData))
import Text.XHtml (height)

x :: Integer
x = 5

greet :: String -> String
-- greet :: [Char] -> [Char]
greet name = "Hello " ++ name

howLong :: [a] -> Integer
howLong [] = 0
howLong (x : xs) = 1 + howLong xs

-- UNION TYPES
-- 'Food' => Type Constructor
-- 'Fruit', 'Dairy', ... => Data Constructors
data Food = Fruit | Dairy | Fishb | MetaData

-- PRODUCT TYPES
data Point2D a = Point2D a a

manhattanDistance
  (Point2D x0 y0)
  (Point2D x1 y1) = (x1 - x0) + (y1 - y0)

p1 = Point2D 0 0

p2 = Point2D 1 1

data Point2D' a = Point2D' {pointX, pointY :: a}

p3 = Point2D' 1 2

-- It gives us two 'selector functions':
-- pointX p3 -> 1
-- pointY p3 -> 2

showJustX (Point2D' x1 y1) = pointX (Point2D' x1 y1)

getJustX Point2D' {pointX = x} = x

-- RECURSIVE TYPES

data Tree a = Leaf a | Branch (Tree a) (Tree a)

myTree =
  Branch
    (Leaf "a")
    (Branch (Leaf "b") (Leaf "c"))

-- SYNONYM TYPES

type MyString = [Char]

name :: MyString
name = "Yuliya"

lst :: [Integer]
lst = (1 : (2 : (3 : [])))

-- Partial functions

add1 = (1 +)

double = (2 *)

myList = [1, 2, 3, 4, 5]

comp = add1 . double

-- map (add1 . double) myList => [3,5,7,9,11]
-- functions are applied from RIGHT to LEFT:
-- map (double . add1) myList => [4,6,8,10,12]

-- LET for local scope variables
cylinderArea r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- sum $ map (\x -> double x) myList
-- sum (map (\x -> double x) myList)

-- '@' DESTRUCTURING in pattern matching
-- it keeps the reference to the whole list,
-- the head, and the rest.

showMyList lst@(x : xs) =
  "Head: "
    ++ show x
    ++ " Rest: "
    ++ show xs
    ++ " List: "
    ++ show lst

infList = [1 ..]

finiteList = [1, 3 .. 15]

take5 = take 5 infList

-- List comprehension
evenNums = [x * 2 | x <- [0 ..]]

-- a^2 + b^2 = c^2
pytagorianTriples =
  [ (a, b, c)
    | c <- [1 ..],
      b <- [1 .. c],
      a <- [1 .. b],
      a ^ 2 + b ^ 2 == c ^ 2
  ]

-- ZIP : takes in parallel one element from each of two lists
-- and makes a pair out of them
-- zip [1, 2, 3, 4, 5] "ciao"
-- [(1,'c'),(2,'i'),(3,'a'),(4,'o')]

-- BOOLEAN GUARD
calcBMI weight height
  | bmi <= underweight = "Underweight"
  | bmi < normal = "Normal"
  | bmi < overweight = "Overweight"
  | otherwise = "Obese"
  where
    bmi = weight / height ^ 2
    -- using pattern matching for assignment
    (underweight, normal, overweight) = (18.5, 25.0, 30.0)

initials firstName lastName = [f] ++ " " ++ [l]
  where
    (f) = head firstName
    (l) = head lastName

initials' firstName lastName = [f] ++ " " ++ [l]
  where
    (f : _) = firstName
    (l : _) = lastName

-- IF
ifExample x = [1, if x > 2 then 999 else 0]

-- "CASE"

head' :: [a] -> a
head' [] = error "Empty list"
head' (x : _) = x

head'' lst = case lst of
  [] -> error "No head"
  (x : xs) -> x

-- Forces evaluation of x
-- seq x y -> y