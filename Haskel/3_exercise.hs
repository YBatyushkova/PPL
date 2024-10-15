import System.Win32 (xBUTTON1, COORD (yPos))
data Result a = Ok a | Err deriving (Eq, Show)

data Expr = Val Int | Div Expr Expr deriving (Eq, Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

ex1 = Div (Val 4) (Val  2)
ex2 = Div (Val 4) (Val  0)

safeDiv :: Int -> Int -> Result Int
safeDiv n m =
    if m == 0
        then Err
        else Ok (n `div` m)

eval' :: Expr -> Result Int
eval' (Val n) = Ok n
eval' (Div x y) = 
    case eval' x of
        Err -> Err
        Ok x -> case eval' y of
            Err -> Err
            Ok y -> safeDiv x y
-- eval' (Div x y) = eval x `safeDiv` eval y

bind :: Result Int -> (Int -> Result Int) -> Result Int
m `bind` f = case m of
    Err -> Err
    Ok x -> f x

eval'' :: Expr -> Result Int
eval'' (Val n) = Ok n
eval'' (Div x y) = 
    eval'' x `bind` (\n -> eval'' y `bind` \m -> safeDiv n m)

mEval :: Expr -> Result Int
mEval (Val n) = Ok n
mEval (Div x y) = do
    n <- mEval x
    m <- mEval y
    safeDiv n m

instance Functor Result where
    fmap f (Ok x) = Ok (f x)
    fmap _ Err = Err

instance Applicative Result where
    pure = Ok
    _ <*> Err = Err
    Err <*> _ = Err
    Ok f <*> Ok x = fmap f (Ok x)

instance Monad Result where
    return = pure
    Err >>= _ = Err
    Ok x >>= f = f x

{--
1. LEFT IDENTITY: `return x >>= f` == `f x`
2. RIGHT IDENTITY: `m >>= return` == `m`
3. ASSOCIATIVITY: `(m >>= f) >>= g` ==
                  `m >>= (\x -> f x >>= g)
--}

----------------

square x = x ^ 2
addOne x = x + 1

x = addOne (square 2)

data NumberWithLogs = NumberWithLogs 
    {
    number :: Int,
    logs :: [String]
    }
    deriving (Eq, Show)

square1 :: Int -> NumberWithLogs
square1 x = NumberWithLogs (x^2) ["Squared " ++ show x ++ " to get " ++ show (x^2)]

square2 :: NumberWithLogs -> NumberWithLogs
square2 x = NumberWithLogs (number x ^ 2) $ logs x ++ ["Squared " ++ show (number x) ++ " to get " ++ show (number x ^ 2)]

addOne1 :: Int -> NumberWithLogs
addOne1 x = NumberWithLogs (x + 1) ["Added 1 to " ++ show x ++ " to get " ++ show (x + 1)]

addOne2 :: NumberWithLogs -> NumberWithLogs
addOne2 x = NumberWithLogs (number x + 1) (logs x ++ ["Added 1 to " ++ show (number x) ++ " to get " ++ show (number x + 1)])


wrapWithLogs :: Int -> NumberWithLogs
wrapWithLogs x = NumberWithLogs x []

runWithLogs :: NumberWithLogs -> (Int -> NumberWithLogs) -> NumberWithLogs
runWithLogs input transform =
    let newNumberWithLogs = transform (number input)
        in NumberWithLogs 
            (number newNumberWithLogs)
            (logs input ++ logs newNumberWithLogs)

a = wrapWithLogs 2
b = runWithLogs a square1
c = b `runWithLogs` addOne1