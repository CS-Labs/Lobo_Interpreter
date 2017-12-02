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
import Data.List.Split
import Language.Haskell.Interpreter

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


-- <input> := <subroutines> <instructions>
-- <subroutines> := <subroutine> <subroutines> | ""
-- <subroutine> := to <symbol> <variable list> [<instructions>]
-- <variable list> := () | ( <variables> )
-- <variables> := <variable> | <variable> , <variables>
-- <instruction> := <0-arity command>
-- <instruction> := <1-arity command> <value>
-- <instruction> := if <boolean> <action> | if <boolean> <action> else <action>
-- <boolean> := <value> = <value> | <value> < <value> | <value> > <value>
-- <action> := <instruction> | [ <instructions> ]
-- <instructions> := <instruction> <instructions> | "
-- <instruction> := repeat <value> <action>
-- <instruction> := make <symbol> <value>
-- <value list> := () | ( <values> )
-- <values> := <value> | <value> , <values>
-- <instruction> := <symbol> <value list>
-- <value> := <E> + <value> | <E> - <value> | <E>
-- <E> := <F> * <E> | <F> / <E> | <F>
-- <F> := <atom> | (<value>)
-- <atom> := <symbol> | <number>
-- <0-arity command> := penup | pendown | stop | push | pop
-- <1-arity command> := forward | backward | left | right | color

data Expression = Mul Data Data
                | Div Data Data
                | Add Data Data
                | Sub Data Data
                | GT Data Data
                | LT Data Data
                | EQ Data Data
                | LTE Data Data
                | GTE Data Data deriving (Show)

data Data = AInt Int | ABool Bool | ADouble Double | AString String | DList [Data] | VarList [(String, Data)] | Variable (String, Data) deriving (Show)

data LocalEnv = LocalEnv {getFuncName :: String, getLocalEnv :: [Data]} deriving (Show)

data Instruction = Penup 
                 | Pendown 
                 | Stop 
                 | Push 
                 | Pop 
                 | Forward Expression  
                 | Backward Expression
                 | Left Expression 
                 | Right Expression
                 | Color Expression 
                 | Repeat Expression [Instruction] -- Action
                 | Make String Data
                 | Call String [Expression]-- Call Subroutine
                 | If Data [Instruction] -- Action
                 | IfElse Data [Instruction] [Instruction] deriving (Show) -- Action Action

data Subroutine = Subroutine String [String] [Instruction] -- Name [Variable Names] [Instruction stream]
newtype JumpTable = JumpTable {getJumpTable :: [(String, Subroutine)]} -- (String, Subroutine)


-- (define starfish
-- '((to starfish (side angle inc)
--     (repeat 90
--       (forward side)
--       (right angle)
--       (make angle (+ angle inc))
--     )
--   )
--   (penup)
--   (forward 50)
--   (pendown)
--   (starfish 30 2 20)
-- ))

-- Resulting data type construction:
-- We start by creating an instance of the subroutine data type which is triggered by the 'to' key word.
-- When the subroutine function is called the variables passed in are bound to the strings used inside
-- of the subroutine. 
-- Our subroutine results as:
-- Subroutine starfish :: String [side :: String, angle :: String, inc :: String] 
-- [Repeat [90 :: Expression] [Forward side :: String, Right angle :: String, Make angle :: String (Add angle :: String inc :: String)]]]
-- Add Subroutine to jump table with (starfish, subroutine)
-- Main Instruction stream:
-- [Penup, forward 50 :: expression, pendown, Call starfish :: string 30 :: Expression 2 :: Expression 20 Expression]

-- Now send the jump table and the main instruction stream to the interpreter
-- The interpeter iterates through the main instruction stream
-- It first hits Penup and calls the Penup function
-- It now hits the forward instruction and calls the appropriate function.
-- It how its the pendown instruction and calls the Pendown function.
-- Now it hits the call instruction. From the call instruction we get the subroutine from the jump table
-- corresponding to the name in the call instruction. Then we call a function that takes a sub expression and it's parameters.
-- When that function is called it binds the variables in the subroutines data type (side, angle and inc) to the parameters (30, 2, 20)
-- When binding the variables it adds it to an instance of the Local enviroment data type which will save the local enviroment
-- corresponding to this subroutine CALL. After that we recusivly call the same function that took our main instruction stream
-- and send it the instruction stream of the subroutine, the same jump table, and the local enviroment. 


-- Convert the s-expression to a stream of instructions and a Jump table of subroutines. 
--preprocessor :: Sexpr -> JumpTable -> [Instruction] -> JumpTable


-- We take the jump table constructed by the s-expression -> instruction translation, the main instruction stream
-- and a local enviroment (empty for initial main instruction stream call)
--interpeter :: JumpTable -> [Instruction] -> LocalEnv -> JumpTable -> [Instruction] -> LocalEnv


stripHeader :: Sexpr -> Sexpr
stripHeader (Cons (Symbol "define") (Cons (Symbol s) (Cons (sexpr) Nil))) = sexpr


-- Main 
-- 1. Read in the file and pass it to the parser p.
-- 2. Parser p returns an s-expression
-- 3. Send the s-expression to the preprocessor and get the main instruction stream and the jump table.
-- 4. Send the main instruction stream, the jump table, and an empty env to the interpeter which will go on to run the program
-- and call instructions in the main instruction stream will result in a recurisve call to the intepreter using the
-- instruction stream corresponding to the called function in the jump table.