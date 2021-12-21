module Final1
  (Id, L0(..), L1(..), L2(..), tagLams, ppL0, ppL1, ppL2, parseL0)
where

import Text.ParserCombinators.ReadP
import Control.Monad

type Id = String

data L0 =
  Var0 Id
  | Lam0 Id L0
  | App0 L0 L0
  deriving (Show, Eq)

data L1 =
  Var1 Int String
  | Lam1 Int Id L1
  | App1 L1 L1
  deriving (Show, Eq)

data L2 =
  Var2 Int
  | Lam2 Int L2
  | App2 L2 L2
  deriving (Show, Eq)

tagLams :: L1 -> L1
tagLams e = fst $ tagLams' e 1

tagLams' :: L1 -> Int -> (L1,Int)
tagLams' (Var1 m x) n =
  (Var1 m x, n)
tagLams' (Lam1 _ x e) n =
  let (e',n') = tagLams' e (n+1)
  in (Lam1 n x e', n')
tagLams' (App1 e1 e2) n = 
  let (e1',n') = tagLams' e1 n
      (e2',n'') = tagLams' e2 n'
  in (App1 e1' e2', n'')

-- Pretty printers and parser below. Ignore the implementation.  You
-- just need:
-- ppL0 :: L0 -> String
-- ppL1 :: L1 -> String  
-- ppL2 :: L2 -> String  
-- parseL0 :: String -> L0
-- (no parsers for L1 and L2)

ppL0 :: L0 -> String
ppL0 (Var0 x) = x
ppL0 (Lam0 x e) = "(" ++ "%" ++ x ++ " -> " ++ ppL0 e ++ ")"
ppL0 (App0 e (Var0 x)) = ppL0 e ++ " " ++ x
ppL0 (App0 e e') = ppL0 e ++ "(" ++ ppL0 e' ++ ")"

numTag :: Int -> String -> String
numTag n s =
  show n ++ ":" ++ s 

ppL1 :: L1 -> String  
ppL1 (Var1 n x) = numTag n x
ppL1 (Lam1 n x e) = "(" ++ numTag n ("%" ++ x ++ " -> " ++ ppL1 e) ++ ")"
ppL1 (App1 e (Var1 n x)) = ppL1 e ++ " " ++ numTag n x
ppL1 (App1 e e') = ppL1 e ++ "(" ++ ppL1 e' ++ ")"

ppL2 :: L2 -> String  
ppL2 (Var2 n) = show n
ppL2 (Lam2 n e) = "(" ++ "%" ++ show n ++ " -> " ++ ppL2 e ++ ")"
ppL2 (App2 e (Var2 n)) = ppL2 e ++ " " ++ show n
ppL2 (App2 e e') = ppL2 e ++ "(" ++ ppL2 e' ++ ")"

parseL0 :: String -> L0

nextChar :: Char -> ReadP ()
nextChar c = skipSpaces >> char c >> return ()

nextString :: String -> ReadP ()
nextString = mapM_ nextChar

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

isAlphanum :: Char -> Bool
isAlphanum =  liftM2 (||) isAlpha isNum
             
idParser :: ReadP String
idParser = do
  skipSpaces
  c1 <- satisfy isAlpha
  rest <- munch isAlphanum
  return $ c1:rest

varParser :: ReadP L0
varParser = do
  x <- idParser
  return $ Var0 x

lamParser :: ReadP L0
lamParser = do
  nextChar '%'
  x <- idParser
  nextString "->"
  e <- lParser
  return $ Lam0 x e

parenParser :: ReadP a -> ReadP a
parenParser p =
  between (nextChar '(') (nextChar ')') p

headParser :: ReadP L0
headParser = varParser +++ (parenParser lParser)

argParser :: ReadP L0
argParser = varParser +++ lamParser +++ (parenParser lParser) 
  
argsParser :: ReadP [L0]
argsParser =
  args1Parser <++ return []

args1Parser :: ReadP [L0]
args1Parser =
  do e <- argParser
     es <- argsParser
     return $ e:es

appParser :: ReadP L0
appParser =
  do e <- headParser
     args <- args1Parser
     return $ foldl1 App0 $ e : args
               
lParser :: ReadP L0
lParser = appParser <++ varParser +++ lamParser +++ (parenParser lParser)

parseWith :: ReadP a -> String -> a
parseWith p = fst . head . readP_to_S p

parseL0 = parseWith lParser



