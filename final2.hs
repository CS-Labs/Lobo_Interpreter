{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
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
import Data.Fixed
import Control.Monad
import Control.Applicative hiding (many)
import Prelude hiding (reverse)
import Data.IORef
import Data.List hiding (reverse)
import Data.Monoid
import Graphics.Rendering.OpenGL hiding (get,scale)
import Graphics.UI.GLUT hiding (get,scale)
import Control.Monad hiding (join)
import Control.Monad.State hiding (join)
import Control.Monad.Writer hiding (join)
import Control.Monad.Trans hiding (join)

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


type Point = (GLfloat, GLfloat)

instance Num Point where (x, y) + (x', y') = (x + x', y + y')

type Colour = (GLfloat, GLfloat, GLfloat)
data Plumber = Plumber Point GLfloat
data Box = Box GLfloat GLfloat GLfloat GLfloat deriving Show


type ColorTriple = (GLfloat, GLfloat, GLfloat)

red   = (1, 0, 0)
green = (0, 1, 0)
blue  = (0, 0, 1)
black = (0, 0, 0)
white = (1, 1, 1)

data Graphic = Straight GLfloat | Invisible GLfloat | Bend GLfloat | Join Graphic Graphic | Fork Graphic Graphic | Paint Colour Graphic deriving Show

myJoin = foldr1 Join
fork = foldr1 Fork

renderStraight :: Point -> Point -> StateT Plumber IO ()
renderStraight (x0, y0) (x1, y1) =
    lift $ renderPrimitive Lines $ mapM_ vertex [Vertex2 x0 y0, Vertex2 x1 y1]

degToRad d = (d / 360) * 2 * pi

move l t' = modify (\(Plumber (x, y) t) -> Plumber (x + l * cos t, y + l * sin t) (t + t'))

render :: Graphic -> StateT Plumber IO ()
render (Straight length) = do
  Plumber p _ <- get
  move length 0
  Plumber q _ <- get
  renderStraight p q

render (Invisible length) = move length 0

render (Bend angle) = move 0 (degToRad angle)

render (Join g g') = do
  render g
  render g'

render (Fork g g') = do
  p <- get
  render g
  put p
  render g'
  put p

render (Paint (r', g', b') g) = do
  lift $ currentColor $= Color4 r' g' b' 1
  render g

instance Monoid Box where
    mempty = Box 0 0 0 0
    Box xMin yMin xMax yMax `mappend` Box x0 y0 x1 y1 =
        Box (min x0 xMin) (min y0 yMin) (max x1 xMax) (max y1 yMax)

forward :: GLfloat -> StateT Plumber (Writer Box) ()
forward length = do
  move length 0
  Plumber (x, y) _ <- get
  tell $ Box x y x y

boundingBox :: Graphic -> StateT Plumber (Writer Box) ()
boundingBox (Straight  length) = forward length
boundingBox (Invisible length) = forward length

boundingBox (Bend angle) = move 0 (degToRad angle)

boundingBox (Join g g') = do
  boundingBox g
  boundingBox g'

boundingBox (Fork g g') = do
  p <- get
  boundingBox g
  put p
  boundingBox g'
  put p

boundingBox (Paint (r', g', b') g) = boundingBox g

mirror (Bend angle)         = Bend (-angle)
mirror (Join g g')          = mirror g `Join` mirror g'
mirror (Fork g g')          = mirror g `Fork` mirror g'
mirror (Paint color g)      = Paint color (mirror g)
mirror g                    = g

reverse (Join g g')        = reverse g' `Join` reverse g
reverse (Fork g g')        = reverse g  `Fork` reverse g'
reverse (Paint color g)    = Paint color (reverse g)
reverse g                  = g

scale s (Straight  length) = Straight  $ s*length
scale s (Invisible length) = Invisible $ s*length
scale s (Join g g')        = scale s g `Join` scale s g'
scale s (Fork g g')        = scale s g `Fork` scale s g'
scale s (Paint color g)    = Paint color (scale s g)
scale s g                  = g

-- Compute bounding box and draw centered figure
draw g = do
  let Box xMin yMin xMax yMax = execWriter (execStateT (boundingBox g) (Plumber (0, 0) 0))
  let (dx, dy) = (xMax - xMin, yMax - yMin)
  let s = 2 / max dx dy
  let (x, y) = (-s * (xMax + xMin) / 2, -s * (yMax + yMin) / 2)
  runStateT (render (scale s g)) (Plumber (x, y) 0)

repeat' n g = myJoin $ genericTake n (repeat g)

polygon n = repeat' n . Join (Straight 1) . Bend $ 360 / (fromInteger n)

-- Koch curve

koch angle 0 = Straight 1
koch angle n = scale 2 . kochStep angle $ koch angle (n-1)

kochStep angle g = myJoin [g, Bend (-angle), g, Bend (2*angle), g, Bend (-angle), g]

-- Gosper curve

gosper 0 = Straight 1
gosper n = gosperStep $ gosper (n-1)

gosperStep g = myJoin [Bend 15, g, Bend (-60), g', Bend (-120), g', Bend 60, g, Bend 120, g, g, Bend 60, g', Bend (-75)]
  where g' = mirror $ reverse g

-- Sierpinski tree

sierpinski 0 = Straight 1
sierpinski n = scale 0.5 . sierpinskiStep $ sierpinski (n-1)

sierpinskiStep g = Straight 2 `Join` fork [Bend (-120) `Join` g, Bend 0 `Join` g, Bend 120 `Join` g]

-- Monkey tree

d = (sqrt 3) / 3

monkey 0 = Straight 1
monkey n = monkeyStep $ monkey (n-1)

monkeyStep g = myJoin [Bend (-60), mirror g, reverse g, Bend 60, g, Bend 60, reverse g, Bend 150, scale d . reverse $ g, scale d . mirror $ g, Bend (-60), scale d . mirror . reverse $ g, Bend (-60), scale d . mirror . reverse $ g, scale d g, Bend (-90), mirror . reverse $ g, g]

-- Logo

starfish angle step = myJoin . concat $ take 90 [[Straight 1, Bend angle] | angle <- [angle, angle + step .. ]]

stars angle = repeat' 5 $ myJoin . concat $ take 8 [[Straight i, Bend angle] | i <- [1..]]

logo n x dx y dy = myJoin . concat $ take n [[Straight (x + i*dx), Bend (y + i*dy)] | i <- [1..]]

starfish' angle step = logo 90 1 0 angle step

stars' angle = repeat' 5 $ logo 8 1 1 angle 0

-- Marroquin pattern

row n = myJoin [repeat' n $ polygon 4 `Join` Invisible 20, Bend 180, Invisible $ fromIntegral n * 20, Bend (-90), Invisible 20, Bend (-90)]

grid n = myJoin [Bend (-135), Invisible $ sqrt 2 * 10 * fromIntegral (n-1), Bend 135, repeat' n $ row n]

marroquin n = fork [Bend 120 `Join` g, Bend 60 `Join` g, g] where g = grid n

-- Wow

wow = scale 1.5 $ repeat' 71 $ (repeat' 360 $ Straight 1 `Join` Bend 1) `Join` Bend 5

--interaction

bindings :: KeyboardMouseCallback
bindings key keystate modifiers positions =
    putStrLn (show (key, keystate, modifiers, positions))

motion :: MotionCallback
motion position = putStrLn (show position)

increment n = if n == 5 then 1 else n + 1
--increment n = if n == 36 then 1 else n + 1


data Expression = Mul Data Data
                | Div Data Data
                | Add Data Data
                | Sub Data Data
                | GT Data Data
                | LT Data Data
                | EQ Data Data
                | LTE Data Data
                | GTE Data Data deriving (Show)

data Data = AGLfloat {getval :: GLfloat} | AInt {getintval :: Int} | ABool Bool | ADouble Double | AString String | DList [Data] | VarList [(String, Data)] | Var String  | Expression deriving (Show)

data LocalEnv = LocalEnv {getFuncName :: String, getLocalEnv :: [Data]} deriving (Show)

type PenState = String

type GraphicsState =  (ColorTriple, PenState, [Graphic])

data Instruction = Penup 
                 | Pendown 
                 | Stop 
                 | Push 
                 | Pop 
                 | Forward Data  
                 | Backward Data
                 | MyLeft Data 
                 | MyRight Data
                 | MyColor Data 
                 | MyRepeat Data [Instruction] -- Action
                 | Make String Data
                 | Call String [Expression]-- Call Subroutine
                 | If Data [Instruction] -- Action
                 | IfElse Data [Instruction] [Instruction] deriving (Show) -- Action Action

data Subroutine = Subroutine String [String] [Instruction] -- Name [Variable Names] [Instruction stream]
newtype JumpTable = JumpTable {getJumpTable :: [(String, Subroutine)]} -- (String, Subroutine)

hueToRGB :: Float -> ColorTriple
hueToRGB hue = let x = (1 - (abs $ ((hue / 60) `mod'` 2) - 1)) in 
    if (hue >= 0 && hue < 60) then (getval (AGLfloat 1),getval (AGLfloat x),getval (AGLfloat 0)) else if
      (hue >= 60 && hue < 120) then (getval (AGLfloat x),getval (AGLfloat 1),getval (AGLfloat 0)) else if 
        (hue >= 120 && hue < 180) then (getval (AGLfloat 0),getval (AGLfloat 1),getval (AGLfloat x)) else if 
          (hue >= 180 && hue < 240) then (getval (AGLfloat 0),getval (AGLfloat x),getval (AGLfloat 1)) else if 
            (hue >= 240 && hue < 300) then (getval (AGLfloat x),getval (AGLfloat 0),getval (AGLfloat 1)) else 
              (getval (AGLfloat 1),getval (AGLfloat 0),getval (AGLfloat x))



stripHeader :: Sexpr -> Sexpr
stripHeader (Cons (Symbol "define") (Cons (Symbol s) (Cons (sexpr) Nil))) = sexpr


preprocessor :: Sexpr -> [Instruction] -> [Instruction]
preprocessor (Nil) instStream = instStream
preprocessor (Cons (Cons (Symbol "penup") Nil) rest) instStream = preprocessor rest (instStream ++ [Penup])
preprocessor (Cons (Cons (Symbol "pendown") Nil) rest) instStream = preprocessor rest (instStream ++ [Pendown])
preprocessor (Cons (Cons (Symbol "forward") (Cons (SexprInt i) Nil)) rest) instStream = preprocessor rest (instStream ++ [Forward (AGLfloat (fromIntegral i))])
preprocessor (Cons (Cons (Symbol "forward") (Cons (Symbol var) Nil)) rest) instStream = preprocessor rest (instStream ++ [Forward (Var var)])
preprocessor (Cons (Cons (Symbol "backward") (Cons (SexprInt i) Nil)) rest) instStream = preprocessor rest (instStream ++ [Backward (AGLfloat (fromIntegral i))])
preprocessor (Cons (Cons (Symbol "backward") (Cons (Symbol var) Nil)) rest) instStream = preprocessor rest (instStream ++ [Backward (Var var)])
preprocessor (Cons (Cons (Symbol "right") (Cons (SexprInt i) Nil)) rest) instStream = preprocessor rest (instStream ++ [MyRight (AGLfloat (fromIntegral i))])
preprocessor (Cons (Cons (Symbol "right") (Cons (Symbol var) Nil)) rest) instStream = preprocessor rest (instStream ++ [MyRight (Var var)])
preprocessor (Cons (Cons (Symbol "left") (Cons (SexprInt i) Nil)) rest) instStream = preprocessor rest (instStream ++ [MyLeft (AGLfloat (fromIntegral i))])
preprocessor (Cons (Cons (Symbol "left") (Cons (Symbol var) Nil)) rest) instStream = preprocessor rest (instStream ++ [MyLeft (Var var)])
preprocessor (Cons (Cons (Symbol "color") (Cons (SexprInt i) Nil)) rest) instStream = preprocessor rest (instStream ++ [MyColor (AInt i)])
preprocessor (Cons (Cons (Symbol "repeat") (Cons (SexprInt i) sexpr)) rest) instStream = preprocessor rest (instStream ++ [MyRepeat (AInt i) (preprocessor sexpr [])])

getGraphicInstStream :: GraphicsState-> [Graphic]
getGraphicInstStream (c,s,graphicInstStream) = graphicInstStream

graphicsTranslator :: [Instruction] -> GraphicsState -> GraphicsState
graphicsTranslator [] gs = gs
graphicsTranslator ((MyColor val):rest) (c,s,g) = graphicsTranslator rest (hueToRGB (fromIntegral (getintval val)),s,g)
graphicsTranslator (Penup:rest) (c,s,g) = graphicsTranslator rest (c,"up",g)
graphicsTranslator (Pendown:rest) (c,s,g) = graphicsTranslator rest (c,"down",g)
graphicsTranslator ((Forward val):rest) (c,s,g) = let gnew = g ++ [Paint c $ (if s == "down" then Straight (getval val) else Invisible (getval val))] in graphicsTranslator rest (c,s,gnew)
graphicsTranslator ((Backward val):rest) (c,s,g) = let gnew = g ++ [Paint c $ (if s == "down" then Straight (-(getval val)) else Invisible (-(getval val)))] in graphicsTranslator rest (c,s,gnew)
graphicsTranslator ((MyRight val):rest) (c,s,g) = let gnew = g ++ [Paint c $ Bend (-(getval val))] in graphicsTranslator rest (c,s,gnew)
graphicsTranslator ((MyLeft val):rest) (c,s,g) = let gnew = g ++ [Paint c $ Bend (getval val)] in graphicsTranslator rest (c,s,gnew)
graphicsTranslator ((MyRepeat (AInt i) inst):rest) (c,s,g) = let (_,_,gnew) = (graphicsTranslator inst (c,s,[])) in graphicsTranslator rest (c,s,(concat (replicate i gnew)))



--testString = "(define foo '((forward 50) (right 90) (forward 25) (right 90) (forward 30)))"
--testString = "(define foo '((forward 50) (left 90) (forward 25) (left 90) (forward 30)))"
-- testString = "(define foo '((forward 50) (right 90) (backward 25) (right 90) (forward 30)))"
testString = "(define foo '((right 30) (color 60) (forward 100) (right 120) (color 300) (forward 100) (right 120) (color 180) (forward 80)))"
-- testString = "(define foo '((repeat 10 (penup) (forward 5) (pendown) (forward 5))))"
-- testString = "(define foo '((repeat 4 (forward 5) (right 90)) (repeat 4 (forward 2) (right 90)))))"

debugGetInstStream = (preprocessor (stripHeader $ p testString) [])
debugGetGraphicsInstStream = ([Bend 90] ++ (getGraphicInstStream (graphicsTranslator (preprocessor (stripHeader $ p testString) []) (white,"down", []))))


main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Haskell Plumbing Graphics"
  graphicInstStream <- newIORef ([Bend 90] ++ (getGraphicInstStream (graphicsTranslator (preprocessor (stripHeader $ p testString) []) (white,"down", []))))
  displayCallback $= display graphicInstStream
  actionOnWindowClose $= MainLoopReturns
  mainLoop

display is = do
  clear [ColorBuffer]
  graphicInstStream <- readIORef is
  putStr $ show graphicInstStream
  draw $ (myJoin graphicInstStream)
  flush


-- Stages now: Sexpr -> [Instruction] -> [Graphic]
-- We can't jump from Sexpr -> [Graphic] or we can't maintain state.
-- We have to use [Graphic] because it's how we can interface with Lances library.
