module Final3 (Fn(..), E(..), Val, DefM, ValM, ST, parseE, parseDefE,
               pop, top, push, runST)
where

import Final2
import Text.ParserCombinators.ReadP

type Id = String

data Fn =
  Succ       -- add one to a number
  | Pred     -- subtract one from a number
  | Defd Id  -- a function with a supplied definition
  deriving Show
                         
data E =
  Const Int
  | Var Id
  | Call Fn [E]  -- apply a function to a list of arguments
  | Ifz E E E    -- if zero ...
  | Push E       -- push the argument onto a global stack
  | Pop          -- pop the global stack, returning the top element
  | Top          -- return the top element of the stack
  deriving Show

type Val = Int

type DefM = Map Id ([Id], E)
type ValM = Map Id Val

type S = [Int]
data ST a = ST (S -> (a, S))

pop :: ST Int
pop =
  ST safePop
  where safePop [] = (-1, [])
        safePop (x:l) = (x,l)

top :: ST Int
top =
  ST safeTop
  where safeTop [] = (-1, [])
        safeTop (x:l) = (x,x:l)

push :: Int -> ST ()
push n = ST (\l -> ((), n:l))

runST :: ST a -> a
runST st =
  let ST f = st
  in fst $ f []

-- ignore
instance Functor ST where
    fmap f (ST st) =
        ST $ \s -> let (x,s') = st s in (f x, s')

-- ignore
instance Applicative ST where
    pure x = ST $ \s -> (x, s)
    (ST st1) <*> (ST st2) = 
        ST (\s -> let (f, s1) = st1 s
                      (x, s2) = st2 s1 in
                    (f x, s2))

-- same definitions as for the "counter" example in class
instance Monad ST where
    (>>=) (ST st1) h = 
        ST $
        \s -> let (x, s1) = st1 s
                  ST st2 = h x
              in st2 s1

------------
-- Parser --
------------

nextChar :: Char -> ReadP Char
nextChar c = skipSpaces >> char c

pfailIf :: Bool -> ReadP ()
pfailIf b = if b then pfail else return ()

isLowerAlpha :: Char -> Bool
isLowerAlpha c = c `elem` "abcdefghijklmnopqrstuvwxyz"

isUpperAlpha :: Char -> Bool
isUpperAlpha c = c `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

isAlpha :: Char -> Bool
isAlpha c = isUpperAlpha c || isLowerAlpha c

isNum :: Char -> Bool
isNum c = c `elem` "0123456789"

isReservedWord :: String -> Bool
isReservedWord s = s `elem` (words "pred succ ifz push top pop")

isAlphanum :: Char -> Bool
isAlphanum x =  isAlpha x || isNum x

idParser :: ReadP String
idParser = do
  skipSpaces
  c1 <- satisfy isAlpha
  rest <- munch isAlphanum
  let result = c1:rest
  pfailIf (isReservedWord result)
  return result

stringParser :: String -> ReadP ()
stringParser =  sequence_ . (map nextChar)

natParser :: ReadP E
natParser =
  do
    skipSpaces
    num <- munch1 isNum
    return $ Const $ (read num :: Int)

varParser :: ReadP E
varParser = do
  id <- idParser
  pfailIf (isReservedWord id)
  return $ Var $ id

atomParser :: ReadP E
atomParser =
    choice [varParser, natParser, popParser, topParser]

fnParser :: ReadP Fn
fnParser =
  let p s f = stringParser s >> return f
  in p "succ" Succ
     +++ p "pred" Pred
     +++ (idParser >>= return . Defd)

parenParser :: ReadP a -> ReadP a
parenParser p =
  between (nextChar '(') (nextChar ')') p

parendListParser :: ReadP a -> ReadP [a]
parendListParser p =
  parenParser $ sepBy p $ nextChar ','
  
ifzParser :: ReadP E
ifzParser = do
  stringParser "ifz"
  [e1,e2,e3] <- parendListParser eParser
  return $ Ifz e1 e2 e3

callParser :: ReadP E
callParser =
  do fn <- fnParser
     es <-parendListParser eParser
     return $ Call fn es

pushParser :: ReadP E
pushParser = do
  stringParser "push"
  e <- parenParser $ eParser
  return $ Push e

popParser :: ReadP E
popParser = do
  stringParser "pop()"
  return $ Pop

topParser :: ReadP E
topParser = do
  stringParser "top()"
  return $ Top

eParser :: ReadP E
eParser = callParser <++ atomParser +++ ifzParser +++ (parenParser eParser)
          +++ popParser +++ pushParser

defParser :: ReadP (Id,([Id],E))
defParser = do
  f <- idParser
  xs <- parendListParser idParser
  nextChar '='
  body <- eParser
  return $ (f, (xs,body))

parseWith :: ReadP a -> String -> a
parseWith p = fst . head . readP_to_S p

parseE :: String -> E
parseE = parseWith eParser

parseDefE :: String -> (Id,([Id],E))
parseDefE = parseWith defParser


class Symantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b

th1 :: Symantics repr => repr Int
th1 = add (int 1) (int 2)

th2 :: Symantics repr => repr (Int -> Int)
th2 = lam (\x -> add x x)

th3 :: Symantics repr => repr ((Int -> Int) -> Int)
th3 = lam (\x -> add (app x (int 1)) (int 2))

newtype R a = R { unR :: a }

instance Symantics R where
    int = R
    add e1 e2 = R (unR e1 + unR e2)

    lam f = R (unR . f . R)
    app e1 e2 = R $ unR e1 (unR e2)

eval :: R a -> a
eval = unR

type VarCounter = Int
newtype S a = S { unS :: VarCounter -> String }

instance Symantics S where
    int x = S $ \_ -> show x
    add e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"

    -- about "(f (S $ const x) _)" part,
    -- recall that "f" is *not* an expression but a function,
    -- in this final embedding, it expects an argument, in this case the argument
    -- is just the variable name we just assigned for pretty print (the value bound to "x")
    -- thus "S $ const x".
    lam f = S $ \h ->
        let x = "x" ++ show h
        in "(\\" ++ x  ++ " -> " ++
           unS (f (S $ const x))
               (succ h) ++ ")"
    app e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

view :: S a -> String
view e = unS e 0

class MulSYM repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolSYM repr where
    bool :: Bool -> repr Bool
    leq :: repr Int -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

class FixSYM repr where
    fix :: (repr a -> repr a) -> repr a

tpow :: (Symantics repr, MulSYM repr, FixSYM repr, BoolSYM repr) => repr (Int -> Int -> Int)
tpow = lam (\x -> fix (\self -> lam (\n ->
                                     if_
                                       (leq n (int 0))
                                       (int 1)
                                       (mul
                                          x
                                          (app self (add n (int (-1))))))))

tpow7 :: (Symantics repr, MulSYM repr, FixSYM repr, BoolSYM repr) => repr (Int -> Int)
tpow7 = lam (\x -> (tpow `app` x) `app` int 7)

tpow72 :: (Symantics repr, MulSYM repr, FixSYM repr, BoolSYM repr) => repr Int
tpow72 = tpow7 `app` int 2

instance MulSYM R where
    mul e1 e2 = R $ unR e1 * unR e2

instance BoolSYM R where
    bool = R
    leq e1 e2 = R $ unR e1 <= unR e2
    if_ be et ee = if unR be then et else ee

instance FixSYM R where
    fix f = R $ fx (unR . f . R)
      where
        -- just like Data.Function.fix
        fx f' = let x = f' x in x

-- no change to the evaluator, "eval tpow72" should work as expected

instance MulSYM S where
    mul e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " * " ++ unS e2 h ++ ")"

instance BoolSYM S where
    bool= S . const . show
    leq e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " <= " ++ unS e2 h ++ ")"
    if_ be et ee = S $ \h ->
        "if " ++ unS be h
        ++ " then " ++ unS et h
        ++ " else " ++ unS ee h

instance FixSYM S where
    -- notice two things:
    -- 1. the similarity between this and "lam" case, in both case a new variable
    --    is introduced and the counter bumps in the same way.
    -- 2. "fix" case has no explicit recursion.
    fix e = S $ \h ->
        let self = "self" ++ show h
        in "(fix " ++ self ++ " . " ++
           unS (e (S $ const self)) (succ h) ++ ")"
