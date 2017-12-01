{---------------------------------------------------------------------

           A HASKELL LIBRARY OF MONADIC PARSER COMBINATORS

                          17th April 1997

               Graham Hutton              Erik Meijer
          University of Nottingham   University of Utrecht

This Haskell 1.3 library is derived from our forthcoming JFP article
"Monadic Parsing in Haskell".  The library also includes a few extra
combinators that were not discussed in the article for reasons of space:

   o force (used to make "many" deliver results lazily);

   o digit, lower, upper, letter, alphanum (useful parsers);

   o ident, nat, int (useful token parsers).

---------------------------------------------------------------------}

module Parselib
   (Parser, item, sat, (+++), string, many, many1, sepby, sepby1,
    chainl, chainl1, char, digit, lower, upper, letter, alphanum,
    symb, ident, nat, int, token, apply, parse, space, integer, natural) where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 +++

-- Monad of parsers: -------------------------------------------------

newtype Parser a = Parser (String -> [(a,String)])

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance Functor Parser where
  fmap = (<$>)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
   return a      = Parser (\cs -> [(a,cs)])
   p >>= f       = Parser (\cs -> concat [parse (f a) cs' |
                                     (a,cs') <- parse p cs])

instance MonadPlus Parser where
   mzero          = Parser (\cs -> [])
   p `mplus` q    = Parser (\cs -> parse p cs ++ parse q cs)

-- Other parsing primitives: -----------------------------------------

parse           :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

item            :: Parser Char
item             = Parser (\cs -> case cs of
                                     ""     -> []
                                     (c:cs) -> [(c,cs)])

sat             :: (Char -> Bool) -> Parser Char
sat p            = do {c <- item; if p c then return c else mzero}

-- Efficiency improving combinators: ---------------------------------

force           :: Parser a -> Parser a
force p          = Parser (\cs -> let xs = parse p cs in
                              (fst (head xs), snd (head xs)) : tail xs)

(+++)           :: Parser a -> Parser a -> Parser a
p +++ q          = Parser (\cs -> case parse (p `mplus` q) cs of
                                     []     -> []
                                     (x:xs) -> [x])

-- Recursion combinators: --------------------------------------------

string          :: String -> Parser String
string ""        = return ""
string (c:cs)    = do {char c; string cs; return (c:cs)}

many            :: Parser a -> Parser [a]
many p           = force (many1 p +++ return [])

many1           :: Parser a -> Parser [a]
many1 p          = do {a <- p; as <- many p; return (a:as)}

sepby           :: Parser a -> Parser b -> Parser [a]
p `sepby` sep    = (p `sepby1` sep) +++ return []

sepby1          :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep   = do {a <- p; as <- many (do {sep; p}); return (a:as)}

chainl          :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a    = (p `chainl1` op) +++ return a

chainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do {a <- p; rest a}
                   where
                      rest a = do {f <- op; b <- p; rest (f a b)}
                               +++ return a

-- Useful parsers: ---------------------------------------------------

char            :: Char -> Parser Char
char c           = sat (c ==)

digit           :: Parser Int
digit            = do {c <- sat isDigit; return (ord c - ord '0')}

lower           :: Parser Char
lower            = sat isLower

upper           :: Parser Char
upper            = sat isUpper

letter          :: Parser Char
letter           = sat isAlpha

alphanum        :: Parser Char
alphanum         = sat isAlphaNum

symb            :: String -> Parser String
symb cs          = token (string cs)

ident           :: [String] -> Parser String
ident css        = do cs <- token identifier
                      guard (not (elem cs css))
                      return cs

identifier      :: Parser String
identifier       = do {c <- lower; cs <- many alphanum; return (c:cs)}

nat             :: Parser Int
nat              = token natural

natural         :: Parser Int
natural          = digit `chainl1` return (\m n -> 10*m + n)

int             :: Parser Int
int              = token integer

integer         :: Parser Int
integer          = do {char '-'; n <- natural; return (-n)} +++ nat

-- Lexical combinators: ----------------------------------------------

space           :: Parser String
space            = many (sat isSpace)

token           :: Parser a -> Parser a
token p          = do {a <- p; space; return a}

apply           :: Parser a -> String -> [(a,String)]
apply p          = parse (do {space; p})

-- Example parser for arithmetic expressions: ------------------------
--
-- expr  :: Parser Int
-- addop :: Parser (Int -> Int -> Int)
-- mulop :: Parser (Int -> Int -> Int)
--
-- expr   = term   `chainl1` addop
-- term   = factor `chainl1` mulop
-- factor = token digit +++ do {symb "("; n <- expr; symb ")"; return n}
--
-- addop  = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
-- mulop  = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}
--
----------------------------------------------------------------------


data Sexpr = Symbol String | SexprInt Int | SexprDouble Double | Nil | Cons Sexpr Sexpr


instance Show Sexpr where
    show (Symbol x) = x
    show (SexprInt x) = show x
    show (SexprDouble x) = show x
    show Nil = "()"
    show (Cons x y) = "(" ++ show x ++ showCdr y ++ ")"


showCdr :: Sexpr -> String
showCdr Nil = ""
showCdr (Cons x Nil) = " " ++ show x
showCdr (Cons x v@(Cons y z)) = " " ++ show x ++ showCdr v
showCdr (Cons x y) = " " ++ show x ++ " . " ++ show y
showCdr x = " . " ++ show x

intNum = (do
  num <- many1 ((sat isDigit))
  return (read num :: Int))


doubNum = (do
  x <- many1 (sat isDigit)
  y <- symb "."
  z <- many1 (sat isDigit)
  return (read (x ++ y ++ z) :: Double))

isMisc x = if x `elem` ['<', '>', '^', '+', '-', '*', '/', '=', '!'] then True else False

misc = sat isMisc

first = lower +++ misc

myDigit = do
  d <- sat isDigit
  return d

symbolic = myDigit +++ first


ah = alphanum +++ (sat isSpace)

quote = do
  x <- symb "\""
  y <- (many ah)
  z <- symb "\""
  return (x ++ y ++ z)


symbol = (do
  x <- first
  y <- token (many symbolic)
  return (x:y)) +++ quote

a = (do {s <- symbol; return $ Symbol s}) +++ (do {n <- doubNum; return $ SexprDouble n}) +++ (do {n <- intNum; return $ SexprInt n})-- +++ (do {symb "\""; s <- test; symb "\""; return $ Symbol s})

s = (do {symb "("  +++ symb "\'("; symb ")"; return Nil}) +++ 
    a +++ 
    (do {symb "(" +++ symb "\'("; x <- (token e); symb ")"; return x}) +++ 
    (do {symb "(" +++ symb "\'("; x <- (token s); symb "."; y <- (token s); symb ")"; return (Cons x y)})


e = (do {symb "(" +++ symb "\'("; x <- (token e); symb ")"; y <- (token e); return (Cons x y)}) +++ 
    (do {x <- (token s); y <- (token e); return (Cons x y)}) +++
    (do {x <- (token s); return (Cons x Nil)})


p str = let result = parse s str in if (length result) == 0 then Symbol "Parse Failed" else fst $ head $ result


data ByteCode = Push {getVal :: Value, getLineNum :: Int}
              | Print {getLineNum :: Int} 
              | Let {getLineNum :: Int}
              | Return {getLineNum :: Int} 
              | Add {getLineNum :: Int}
              | Sub {getLineNum :: Int} 
              | Mult {getLineNum :: Int}
              | Div {getLineNum :: Int} deriving (Show)


data Value = ValueInt Int
           | ValueDouble Double
           | ValueString String
           | ValueList [Value] 

instance Show Value where
  show (ValueInt n) = show n
  show (ValueDouble d) = show d
  show (ValueString s) = s
  show (ValueList l) = show l


newtype Env = Env {getEnv :: [(String, Value)]} deriving (Show)

newtype Frame = Frame {getFrame :: [Value]} deriving (Show)

updateEnv v@(str, val) oldEnv = if str `elem` [s1 | (s1,v1) <- oldEnv] 
  then  Env (map (\(s,v) -> if s == str then (s, val) else (s,v)) oldEnv)
  else  Env ([v] ++ oldEnv)


compile :: Sexpr -> [ByteCode] -> [ByteCode]
compile (Cons (Symbol "define") (Cons (Symbol s) sexpr)) bcodes = compile sexpr bcodes
compile (Nil) bcodes = bcodes
compile (Cons (SexprInt n) (Cons (Symbol "print") (Cons (Symbol ('"' : s)) sexpr))) bcodes = 
  compile sexpr (bcodes ++ [(Push (ValueString (filter (/= '"') s)) n), (Print n)])
compile (Cons (SexprInt n) (Cons (Symbol "print") (Cons (SexprInt i) sexpr))) bcodes = 
  compile sexpr (bcodes ++ [(Push (ValueInt i) n), (Print n)])
compile (Cons (SexprInt n) (Cons (Symbol "print") (Cons (SexprDouble d) sexpr))) bcodes = 
  compile sexpr (bcodes ++ [(Push (ValueDouble d) n), (Print n)])


push :: Value -> [Value] -> [Value]
push val frame = [val] ++ frame

vm :: [ByteCode] -> [Value] -> IO ()
vm [] _ = putStr ""
vm ((Push val n):rest) frame = let updatedFrame = (push val frame) in (vm rest updatedFrame)
vm ((Print n):rest) frame = do {myPrint frame; vm rest []}


myPrint frame = putStrLn (show (head frame))

