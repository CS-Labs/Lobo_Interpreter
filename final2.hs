{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
import Data.Char
import Data.Fixed
import Control.Monad
import Parselib
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

data Sexpr = Symbol String | SexprInt Int | SexprFloat Float | Nil | Cons Sexpr Sexpr


instance Show Sexpr where
    show (Symbol x) = x
    show (SexprInt x) = show x
    show (SexprFloat x) = show x
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
  return (read (x ++ y ++ z) :: Float))

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

a = (do {s <- symbol; return $ Symbol s}) +++ (do {n <- doubNum; return $ SexprFloat n}) +++ (do {n <- intNum; return $ SexprInt n})-- +++ (do {symb "\""; s <- test; symb "\""; return $ Symbol s})

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
radToDeg r = (r * 180) / pi

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


data Expression = MyGT Data Data
                | MyLT Data Data
                | MyEQ Data Data
                | LTE Data Data
                | GTE Data Data deriving (Show)

data Data = AGLfloat {getval :: GLfloat} | AInt {getintval :: Int} | ABool Bool | AFloat Float | AString {getstrval :: String} | DList [Data] | VarList [(String, Data)] | Var {getvarstr :: String}  | Expression deriving (Show)

--data LocalEnv = LocalEnv {getFuncName :: String, getLocalEnv :: [Data]} deriving (Show)
type LocalEnv = [(String, Data)]

type PenState = String

type GraphicsState =  (ColorTriple, PenState, (Float,Float,Float), [Graphic])

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
                 | NoOp {unwrap :: Data} -- Perform no operation, just hold data. 
                 | SetXY Data Data 
                 | MyRepeat Data [Instruction] -- Action
                 | Make String Data
                 | Call String [Instruction]-- Call Subroutine
                 | If Expression [Instruction] -- Action
                 | IfElse Expression [Instruction] [Instruction] -- Action Action
                 | Arithmetic Sexpr
                 | MyMul Data Data
                 | MyDiv Data Data
                 | MyAdd Data Data
                 | MySub Data Data deriving (Show)

type Subroutine =  ([Instruction], [Instruction])  -- Name [Variable Names] [Instruction stream]
type JumpTable = [(String, Subroutine)]

hueToRGB :: Float -> ColorTriple
hueToRGB hue = let x = (1 - (abs $ ((hue / 60) `mod'` 2) - 1)) in 
    if (hue >= 0 && hue < 60) then (getval (AGLfloat 1),getval (AGLfloat x),getval (AGLfloat 0)) else if
      (hue >= 60 && hue < 120) then (getval (AGLfloat x),getval (AGLfloat 1),getval (AGLfloat 0)) else if 
        (hue >= 120 && hue < 180) then (getval (AGLfloat 0),getval (AGLfloat 1),getval (AGLfloat x)) else if 
          (hue >= 180 && hue < 240) then (getval (AGLfloat 0),getval (AGLfloat x),getval (AGLfloat 1)) else if 
            (hue >= 240 && hue < 300) then (getval (AGLfloat x),getval (AGLfloat 0),getval (AGLfloat 1)) else 
              (getval (AGLfloat 1),getval (AGLfloat 0),getval (AGLfloat x))

updatePoint :: (Float, Float, Float) -> String -> Float -> (Float, Float, Float)
updatePoint (x,y,a) "F" n = (x+(n*cos(degToRad x)),(y+(n*sin(degToRad y))), a)
updatePoint (x,y,a) "B" n = (x+(n*cos(degToRad x)),(y+(n*sin(degToRad y))),a)
updatePoint (x,y,a) "R" n = (x,y,a+n)
updatePoint (x,y,a) "L" n = (x,y,a-n)



resolveVar :: LocalEnv -> String -> Data
resolveVar env var = head $ [val | (vartmp, val) <- env, var == vartmp]

getSubRoute :: String -> JumpTable -> Subroutine
getSubRoute func jt = head $ [subroute | (funcName, subroute) <- jt, funcName == func]

getParams :: Subroutine -> [String]
getParams (params, ops) = [getvarstr $ unwrap var | var <- params]

getSubRouteInst :: Subroutine -> [Instruction]
getSubRouteInst (params, ops) = ops

resolveArgs :: [Instruction] -> [Data]
resolveArgs inst = [unwrap val | val <- inst]

-- -- To call function pass: zip vars values
updateEnv :: [(String, Data)] -> LocalEnv -> LocalEnv
updateEnv [] env = env
updateEnv bindings@((var, val):xs) env = updateEnv xs (updateEnvHelper var val env)
  
updateEnvHelper :: String -> Data -> LocalEnv -> LocalEnv
updateEnvHelper var val env = if var `elem` [s | (s,v) <- env]
  then map (\(s,v) -> if s == var then (s, val) else (s,v)) env
  else [(var,val)] ++ env


getAngle :: Floating a => (a, a, a) -> (a, a) -> a
getAngle (x0,y0,a) (x,y) = (radToDeg (atan((y-y0)/(x-x0)))) - a

getDist :: Floating a => (a, a, t) -> (a, a) -> a
getDist (x0,y0,a) (x,y) = (sqrt((x-x0)^2 + (y-y0)^2))

stripHeader :: Sexpr -> Sexpr
stripHeader (Cons (Symbol "define") (Cons (Symbol s) (Cons (sexpr) Nil))) = sexpr

stripJumpTable :: ([Instruction], JumpTable) -> [Instruction]
stripJumpTable (inst,jt) = inst

preprocessor :: Sexpr -> ([Instruction], JumpTable) -> ([Instruction], JumpTable)
preprocessor (Nil) (instStream, jt) = (instStream, jt)
preprocessor (Cons (SexprInt val) Nil) (instStream, jt) = ((instStream ++ [NoOp (AGLfloat (fromIntegral val))]), jt)
preprocessor (Cons (SexprFloat val) Nil) (instStream, jt) = ((instStream ++ [NoOp (AGLfloat val)]), jt)
preprocessor (Cons (SexprInt val) rest) (instStream, jt) = preprocessor rest ((instStream ++ [NoOp (AGLfloat (fromIntegral val))]), jt)
preprocessor (Cons (SexprFloat val) rest) (instStream, jt) = preprocessor rest ((instStream ++ [NoOp (AGLfloat val)]), jt)
preprocessor (Cons (Cons (Symbol "penup") Nil) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Penup]),jt)
preprocessor (Cons (Cons (Symbol "pendown") Nil) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Pendown]),jt)
preprocessor (Cons (Cons (Symbol "forward") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Forward (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "forward") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Forward (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "backward") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Backward (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "backward") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Backward (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "right") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyRight (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "right") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyRight (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "left") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyLeft (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "left") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyLeft (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "color") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyColor (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "color") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyColor (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "color") (Cons (SexprFloat i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyColor (AGLfloat i)]),jt)
preprocessor (Cons (Cons (Symbol "repeat") (Cons (SexprInt i) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyRepeat (AInt i) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "setxy") (Cons (SexprInt i1) (Cons (SexprInt i2) Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [SetXY (AInt i1) (AInt i2)]),jt)

-- Sub routines --
preprocessor (Cons (Cons (Symbol "to") (Cons (Symbol funcName) (Cons (Cons (Symbol arg1) Nil) sexpr))) rest) (instStream, jt) = preprocessor rest (instStream, updatedJt)
  where subroute =  (stripJumpTable $ preprocessor sexpr ([], jt)) 
        params = [NoOp (Var arg1)]
        updatedJt = jt ++ [(funcName, (params, subroute))]
preprocessor (Cons (Cons (Symbol "to") (Cons (Symbol funcName) (Cons (Cons (Symbol arg1) (Cons (Symbol arg2) Nil)) sexpr))) rest) (instStream, jt) =  preprocessor rest (instStream, updatedJt)
  where subroute =  (stripJumpTable $ preprocessor sexpr ([], jt)) 
        params = [NoOp (Var arg1), NoOp (Var arg2)]
        updatedJt = jt ++ [(funcName, (params, subroute))]
preprocessor (Cons (Cons (Symbol "to") (Cons (Symbol funcName) (Cons (Cons (Symbol arg1) (Cons (Symbol arg2) (Cons (Symbol arg3) Nil))) sexpr))) rest)  (instStream, jt) =  preprocessor rest (instStream, updatedJt)
  where subroute =  (stripJumpTable $ preprocessor sexpr ([], jt)) 
        params = [NoOp (Var arg1), NoOp (Var arg2), NoOp (Var arg3)]
        updatedJt = jt ++ [(funcName, (params, subroute))]
preprocessor (Cons (Cons (Symbol "to") (Cons (Symbol funcName) (Cons (Cons (Symbol arg1) (Cons (Symbol arg2) (Cons (Symbol arg3) (Cons (Symbol arg4) Nil)))) sexpr))) rest)  (instStream, jt)=  preprocessor rest (instStream, updatedJt)
  where subroute =  (stripJumpTable $ preprocessor sexpr ([], jt)) 
        params = [NoOp (Var arg1), NoOp (Var arg2), NoOp (Var arg3), NoOp (Var arg4)]
        updatedJt = jt ++ [(funcName, (params, subroute))]

--if else with variable and int
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "=") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AInt d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AInt d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AInt d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<=") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AInt d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">=") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AInt d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
--if else with variable and double
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "=") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<=") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">=") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
--if else with 2 variables
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "=") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (Var d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (Var d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (Var d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<=") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (Var d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">=") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) (Cons sexpr1 (Cons sexpr2 Nil)))) rest) (instStream, jt)  = 
  preprocessor rest ((instStream ++ [IfElse (MyEQ (Var d1) (Var d2)) (stripJumpTable $ preprocessor (Cons sexpr1 Nil) ([],jt)) (stripJumpTable $ preprocessor (Cons sexpr2 Nil) ([],jt))]),jt)

--if with variable and int
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "=") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyEQ (Var d1) (AInt d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyLT (Var d1) (AInt d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyGT (Var d1) (AInt d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<=") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (LTE (Var d1) (AInt d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">=") (Cons (Symbol d1) (Cons (SexprInt d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (GTE (Var d1) (AInt d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
--if with 2 variables
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "=") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyEQ (Var d1) (Var d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyLT (Var d1) (Var d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyGT (Var d1) (Var d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<=") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (LTE (Var d1) (Var d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">=") (Cons (Symbol d1) (Cons (Symbol d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (GTE (Var d1) (Var d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
--if with variable and double
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "=") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyEQ (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyLT (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (MyGT (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol "<=") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (LTE (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") (Cons (Cons (Symbol ">=") (Cons (Symbol d1) (Cons (SexprFloat d2) Nil))) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [If (GTE (Var d1) (AFloat d2)) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)

--arithmetic
preprocessor (Cons (Cons (Symbol "+") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Arithmetic (Cons (Symbol "+") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol "-") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Arithmetic (Cons (Symbol "-") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol "*") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Arithmetic (Cons (Symbol "*") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol "/") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Arithmetic (Cons (Symbol "/") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)

-- Any non-matched symbols should be function calls. 
preprocessor (Cons (Cons (Symbol fCall) sexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Call fCall (stripJumpTable $ preprocessor sexpr ([], jt))]) ,jt)

getGraphicInstStream :: GraphicsState-> [Graphic]
getGraphicInstStream (c,s,p,graphicInstStream) = graphicInstStream


graphicsTranslator :: [Instruction] -> GraphicsState -> JumpTable -> LocalEnv -> GraphicsState
graphicsTranslator [] gs jt env = gs
graphicsTranslator ((MyColor (Var var)):rest) (c,s,p,g) jt env = graphicsTranslator rest (cnew,s,p,g) jt env
  where val = getval $ resolveVar env var
        cnew = hueToRGB val
graphicsTranslator ((MyColor val):rest) (c,s,p,g) jt env = graphicsTranslator rest (hueToRGB (getval val),s,p,g) jt env
graphicsTranslator (Penup:rest) (c,s,p,g) jt env = graphicsTranslator rest (c,"up",p,g) jt env
graphicsTranslator (Pendown:rest) (c,s,p,g) jt env = graphicsTranslator rest (c,"down",p,g) jt env
graphicsTranslator ((Forward (Var var)):rest) (c,s,p,g) jt env =  graphicsTranslator rest (c,s,pnew,gnew) jt env
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "F" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight val else Invisible val)]
graphicsTranslator ((Forward val):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,pnew,gnew) jt env
  where pnew = (updatePoint p "F" ((getval val) :: Float))
        gnew = g ++ [Paint c $ (if s == "down" then Straight (getval val) else Invisible (getval val))]
graphicsTranslator ((Backward (Var var)):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,pnew,gnew) jt env
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "B" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight (-val) else Invisible (-val))]
graphicsTranslator ((Backward val):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,pnew,gnew) jt env
  where pnew = (updatePoint p "B" ((getval val) :: Float))
        gnew = g ++ [Paint c $ (if s == "down" then Straight (-(getval val)) else Invisible (-(getval val)))]
graphicsTranslator ((MyRight (Var var)):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,pnew,gnew) jt env
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "R" val)
        gnew = g ++ [Paint c $ Bend (-val)]
graphicsTranslator ((MyRight val):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,pnew,gnew) jt env
  where pnew = (updatePoint p "R" ((getval val) :: Float))
        gnew = g ++ [Paint c $ Bend (-(getval val))]
graphicsTranslator ((MyLeft (Var var)):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,pnew,gnew) jt env
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "L" val)
        gnew = g ++ [Paint c $ Bend val]
graphicsTranslator ((MyLeft val):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,pnew,gnew) jt env
  where pnew = (updatePoint p "L" ((getval val) :: Float))
        gnew = g ++ [Paint c $ Bend (getval val)]
graphicsTranslator ((MyRepeat (AInt i) inst):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,p,gnew) jt env
  where (_,_,_, gOneIter) = (graphicsTranslator inst (c,s,p,[])) jt env
        gnew = g ++ (concat $ replicate i gOneIter)
graphicsTranslator ((SetXY a b):rest) (c,s,p@(x,y,oldang),g) jt env = graphicsTranslator rest (c,s,pnew,gnew) jt env
  where fltpnt = ((fromIntegral (getintval a)),(fromIntegral (getintval b)))
        ang = getAngle p fltpnt
        dist = getDist p fltpnt
        gnew = g ++ [(Bend $ ang), (if s == "down" then Straight dist else Invisible dist), (Bend $ -ang)]
        pnew = ((fromIntegral (getintval a)), (fromIntegral (getintval b)), oldang)
graphicsTranslator ((Call funcName args):rest) (c,s,p,g) jt env = graphicsTranslator rest (c,s,p,gnew) jt env
  where subProc = getSubRoute funcName jt
        params = getParams subProc
        subProcInst = getSubRouteInst subProc
        resolvedArgs = resolveArgs args
        bindings = zip params resolvedArgs
        subProcEnv = updateEnv bindings []
        (_,_,_,gnew) = graphicsTranslator subProcInst (c,s,p,g) jt subProcEnv



-- When hitting a call
-- Get the corresponding subprocess from the jump table
-- Recursively call the translator with the new enviroment and instruction list


--testString = "(define foo '((forward 50) (right 90) (forward 25) (right 90) (forward 30)))"
--testString = "(define foo '((forward 50) (left 90) (forward 25) (left 90) (forward 30)))"
--testString = "(define foo '((forward 50) (right 90) (backward 25) (right 90) (forward 30)))"
-- testString = "(define foo '((right 30) (color 60) (forward 100) (right 120) (color 300) (forward 100) (right 120) (color 180) (forward 80)))"
-- testString = "(define foo '((repeat 10 (penup) (forward 5) (pendown) (forward 5))))"
-- testString = "(define foo '((repeat 4 (forward 5) (right 90)) (repeat 4 (forward 2) (right 90)))))"
--testString = "(define foo '((forward 10) (if (>= n 5.5) (* (cos (+ (* t a) c)) 75) (+ (* (sin (* t b)) 75) 100)))))"
--testString = "(define foo '((right 30) (color 60) (forward 100) (right 120) (color 300) (forward 100) (right 120) (color 180) (forward 80)))"
--testString = "(define foo '((repeat 10 (penup) (forward 5) (pendown) (forward 5))))"
-- testString = "(define foo '((repeat 4 (forward 5) (right 90)) (repeat 4 (forward 2) (right 90)))))"
--testString = "(define foo '((forward 10) (penup) (setxy 20 20) (pendown) (forward 10) (right 135) (forward 10)))"
-- testString = "(define foo '((+ 1 (+1 2))))"
--testString = "(define foo '((to testsub (arg) (forward arg)) (testsub 10)))"
--testString = "(define foo '((to testsub (arg1 arg2) (forward arg1) (right arg2) (forward 5) (right arg2) (forward arg1)) (testsub 10 90)))"
--testString = "(define foo '((to testsub (arg1 arg2 arg3) (forward arg1) (right arg2) (forward 5) (right arg2) (color arg3) (forward arg1)) (testsub 10 90 200)))"
--testString = "(define foo '((to testsub (arg1 arg2 arg3 arg4) (forward arg1) (right arg2) (color arg3) (forward arg1) (right arg2) (forward arg4)) (testsub 10 90 232 80)))"
--testString = "(define foo '((to testsub (arg col) (make col 200) (color col) (forward arg)) (testsub 10 25)))"
--testString = "(define foo '((to testsub (arg col) (make col 200) (color col) (forward arg)) (testsub 10 25)))"



(debugGetInstStream, debugJt) = (preprocessor (stripHeader $ p testString) ([],[]))

debugGetGraphicsInstStream = ([Bend 90] ++ (getGraphicInstStream (graphicsTranslator debugGetInstStream (white,"down",(0.0,0.0,0.0),[]) debugJt [])))
    where (debugGetInstStream, debugJt) = (preprocessor (stripHeader $ p testString) ([],[]))


main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Haskell Plumbing Graphics"
  let (instructionStream, jt) = (preprocessor (stripHeader $ p testString) ([],[]))
  let graphicsInstructionStream = ([Bend 90] ++ (getGraphicInstStream (graphicsTranslator instructionStream (white,"down",(0.0,0.0,0.0),[]) jt [])))
  wrappedInstStream <- newIORef graphicsInstructionStream
  displayCallback $= display wrappedInstStream
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
