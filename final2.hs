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

data Sexpr = Symbol String | SexprInt Int | SexprFloat Float | Nil | Cons {car :: Sexpr, cdr :: Sexpr} 


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
  return (read num :: Int)) +++ (do
    s <- char '-'
    n <- many1 myDigit
    return (read ([s] ++ n) :: Int))


doubNum = (do
  x <- many1 (sat isDigit)
  y <- symb "."
  z <- many1 (sat isDigit)
  return (read (x ++ y ++ z) :: Float)) +++ (do
    s <- char '-'
    x <- many1 (sat isDigit)
    y <- symb "."
    z <- many1 (sat isDigit)
    return (read ([s] ++ x ++ y ++ z) :: Float))

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

a = (do {n <- doubNum; return $ SexprFloat n}) +++ (do {n <- intNum; return $ SexprInt n}) +++ (do {s <- symbol; return $ Symbol s})-- +++ (do {symb "\""; s <- test; symb "\""; return $ Symbol s})

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

data Data = AGLfloat {getval :: GLfloat} 
          | AInt {getintval :: Int} 
          | ABool Bool 
          | AFloat Float 
          | AString {getstrval :: String} 
          | DList [Data] 
          | VarList [(String, Data)] 
          | Var {getvarstr :: String}  
          | Expression 
          | Arithmetic {getsexpr :: Sexpr}
          | Conditional Sexpr deriving (Show)

--data LocalEnv = LocalEnv {getFuncName :: String, getLocalEnv :: [Data]} deriving (Show)
type LocalEnv = [(String, Data)]

type PenState = String


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
                 | If Data [Instruction] -- Action
                 | IfElse Data [Instruction] [Instruction] -- Action Action
                 | ArithWrapper {getexpr :: Data} 
                 | CondWrapper {getcond :: Data}
                 | MyMul Data Data
                 | MyDiv Data Data
                 | MyAdd Data Data
                 | MySub Data Data deriving (Show)

type GraphicsState =  ([Instruction], [Instruction], ColorTriple, PenState, (Float,Float,Float), [Graphic], JumpTable, LocalEnv)


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

isVar :: Data -> Bool 
isVar (Var _) = True
isVar _     = False

isArith :: Data -> Bool
isArith  (Arithmetic _) = True
isArith _ = False

isNoOp :: Instruction -> Bool
isNoOp (NoOp _) = True
isNoOp _ = False

isNil :: Sexpr -> Bool
isNil (Nil) = True
isNil _ = False

resolveArgs :: [Instruction] -> LocalEnv -> [Data]
resolveArgs inst env = [if isVar v then resolveVar env (getvarstr v) else v | v <- [if isNoOp val then unwrap val else getexpr val | val <- inst]]

resolveArithArgs :: [Data] -> LocalEnv -> [Data]
resolveArithArgs args env = [if isArith arg then arithmeticSolver (getsexpr arg) env else arg | arg <- args]

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
preprocessor (Cons (Symbol val) Nil) (instStream, jt) = ((instStream ++ [NoOp (Var val)], jt))
preprocessor (Cons (SexprInt val) rest) (instStream, jt) = preprocessor rest ((instStream ++ [NoOp (AGLfloat (fromIntegral val))]), jt)
preprocessor (Cons (SexprFloat val) rest) (instStream, jt) = preprocessor rest ((instStream ++ [NoOp (AGLfloat val)]), jt)
preprocessor (Cons (Symbol val) rest) (instStream, jt) = preprocessor rest ((instStream ++ [NoOp (Var val)]), jt)
preprocessor (Cons (Cons (Symbol "penup") Nil) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Penup]),jt)
preprocessor (Cons (Cons (Symbol "pendown") Nil) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Pendown]),jt)
preprocessor (Cons (Cons (Symbol "forward") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Forward (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "forward") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Forward (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "forward") arithSexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Forward $ getexpr $ head (stripJumpTable $ preprocessor arithSexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "backward") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Backward (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "backward") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Backward (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "backward") arithSexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Backward $ getexpr $ head (stripJumpTable $ preprocessor arithSexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "right") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyRight (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "right") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyRight (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "right") arithSexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ [MyRight $ getexpr $ head (stripJumpTable $ preprocessor arithSexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "left") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyLeft (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "left") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyLeft (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "left") arithSexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ [MyLeft $ getexpr $ head (stripJumpTable $ preprocessor arithSexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "color") (Cons (Symbol var) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyColor (Var var)]),jt)
preprocessor (Cons (Cons (Symbol "color") (Cons (SexprInt i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyColor (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "color") (Cons (SexprFloat i) Nil)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyColor (AGLfloat i)]),jt)
preprocessor (Cons (Cons (Symbol "color") arithSexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ [MyColor $ getexpr $ head (stripJumpTable $ preprocessor arithSexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "repeat") (Cons (SexprInt i) sexpr)) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [MyRepeat (AInt i) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "repeat") (Cons (Symbol var) sexpr)) rest) (instStream, jt) = preprocessor rest ((instStream ++ [MyRepeat (Var var) (stripJumpTable $ preprocessor sexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "setxy") (Cons (SexprInt i1) (Cons (SexprInt i2) Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [SetXY (AInt i1) (AInt i2)]),jt)
preprocessor (Cons (Cons (Symbol "make") (Cons (Symbol var) (Cons (SexprInt i) Nil))) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Make var (AGLfloat (fromIntegral i))]),jt)
preprocessor (Cons (Cons (Symbol "make") (Cons (Symbol var) (Cons (SexprFloat i) Nil))) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Make var (AGLfloat i)]),jt)
preprocessor (Cons (Cons (Symbol "make") (Cons (Symbol var) arithSexpr)) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Make var $ getexpr $ head (stripJumpTable $ preprocessor arithSexpr ([],jt))]),jt)
preprocessor (Cons (Cons (Symbol "if") sexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ inst), jt) 
  where a = car sexpr
        b = car $ cdr sexpr
        c = cdr $ cdr sexpr
        cond = getcond $ head $ stripJumpTable $ preprocessor (Cons a Nil) ([],jt)
        ifinst = stripJumpTable $ preprocessor (Cons b Nil) ([],jt)
        inst = (if (isNil c) then [If cond ifinst] else [IfElse cond ifinst (stripJumpTable $ preprocessor (Cons c Nil) ([],jt))])

-- Sub routines --
---TODO I just realized this can be converted to one line that takes a variable amount of arguments like call; do later. 
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

--arithmetic
preprocessor (Cons (Cons (Symbol "+") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [ArithWrapper $ Arithmetic (Cons (Symbol "+") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol "-") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [ArithWrapper $ Arithmetic (Cons (Symbol "-") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol "*") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [ArithWrapper $ Arithmetic (Cons (Symbol "*") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol "/") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [ArithWrapper $ Arithmetic (Cons (Symbol "/") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol "-") (Cons sexpr Nil)) rest) (instStream,jt) = preprocessor rest ((instStream ++ [ArithWrapper $ Arithmetic (Cons (Symbol "-") (Cons (SexprFloat 0.0) (Cons sexpr Nil)))]),jt)

--conditionals
preprocessor (Cons (Cons (Symbol "<") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt) = preprocessor rest ((instStream ++ [CondWrapper $ Conditional (Cons (Symbol "<") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol ">") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt) = preprocessor rest ((instStream ++ [CondWrapper $ Conditional (Cons (Symbol ">") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)
preprocessor (Cons (Cons (Symbol "=") (Cons sexpr1 (Cons sexpr2 Nil))) rest) (instStream, jt) = preprocessor rest ((instStream ++ [CondWrapper $ Conditional (Cons (Symbol "=") (Cons sexpr1 (Cons sexpr2 Nil)))]),jt)

-- Any non-matched symbols should be function calls. 
preprocessor (Cons (Cons (Symbol fCall) sexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Call fCall (stripJumpTable $ preprocessor sexpr ([], jt))]) ,jt)

-- Triple nested cons are reduced by one level.
preprocessor (Cons rest nil) (instStream, jt) = preprocessor rest (instStream,jt)  

test (Cons (Cons (Symbol fCall) sexpr) rest) = sexpr

-- test (Cons (Cons (Symbol "if") sexpr) rest) (test,jt) = (inst,jt)
--   where a = car sexpr
--         b = car $ cdr sexpr
--         c = cdr $ cdr sexpr
--         cond = getcond $ head $ stripJumpTable $ preprocessor (Cons a Nil) ([],[])
--         ifinst = stripJumpTable $ preprocessor (Cons b Nil) ([],[])
--         inst = (if (isNil c) then [If cond ifinst] else [IfElse cond ifinst (stripJumpTable $ preprocessor (Cons c Nil) ([],[]))])

getGraphicInstStream :: GraphicsState-> [Graphic]
getGraphicInstStream (_,_,_,_,_,graphicInstStream,_,_) = graphicInstStream


dup :: ([Instruction], [Instruction], ColorTriple, PenState, (Float,Float,Float), [Graphic], JumpTable, LocalEnv) -> ([Instruction], [Instruction], ColorTriple, PenState, (Float,Float,Float), [Graphic], JumpTable, LocalEnv)
dup (a,b,c,d,e,f,g,h) = (b,b,c,d,e,f,g,h)


-- For nested arithmetic expressions try calling solver recursively.
-- TODO; there is probably a more concise way to do this
arithmeticSolver :: Sexpr -> LocalEnv -> Data
-- Given Just constants.
arithmeticSolver (Cons (Symbol "+") (Cons (SexprInt i1) (Cons (SexprInt i2) Nil))) env = (AGLfloat (fromIntegral (i1+i2)))
arithmeticSolver (Cons (Symbol "-") (Cons (SexprInt i1) (Cons (SexprInt i2) Nil))) env = (AGLfloat (fromIntegral (i1-i2)))
arithmeticSolver (Cons (Symbol "*") (Cons (SexprInt i1) (Cons (SexprInt i2) Nil))) env = (AGLfloat (fromIntegral (i1*i2)))
arithmeticSolver (Cons (Symbol "/") (Cons (SexprInt i1) (Cons (SexprInt i2) Nil))) env = (AGLfloat  ((fromIntegral i1)/(fromIntegral i2)))
arithmeticSolver (Cons (Symbol "+") (Cons (SexprFloat f1) (Cons (SexprFloat f2) Nil))) env = (AGLfloat (f1+f2))
arithmeticSolver (Cons (Symbol "-") (Cons (SexprFloat f1) (Cons (SexprFloat f2) Nil))) env = (AGLfloat (f1-f2))
arithmeticSolver (Cons (Symbol "*") (Cons (SexprFloat f1) (Cons (SexprFloat f2) Nil))) env = (AGLfloat (f1*f2))
arithmeticSolver (Cons (Symbol "/") (Cons (SexprFloat f1) (Cons (SexprFloat f2) Nil))) env = (AGLfloat (f1/f2))
-- With variables.
arithmeticSolver (Cons (Symbol "+") (Cons (SexprInt i1) (Cons (Symbol s2) Nil))) env = let i2 = getval $ resolveVar env s2 in AGLfloat((fromIntegral i1) +i2)
arithmeticSolver (Cons (Symbol "-") (Cons (SexprInt i1) (Cons (Symbol s2) Nil))) env = let i2 = getval $ resolveVar env s2 in AGLfloat((fromIntegral i1)-i2)
arithmeticSolver (Cons (Symbol "*") (Cons (SexprInt i1) (Cons (Symbol s2) Nil))) env = let i2 = getval $ resolveVar env s2 in AGLfloat((fromIntegral i1)*i2)
arithmeticSolver (Cons (Symbol "/") (Cons (SexprInt i1) (Cons (Symbol s2) Nil))) env = let i2 = getval $ resolveVar env s2 in AGLfloat((fromIntegral i1)/i2)
arithmeticSolver (Cons (Symbol "+") (Cons (SexprFloat f1) (Cons (Symbol s2) Nil))) env = let f2 = getval $ resolveVar env s2 in AGLfloat(f1+f2)
arithmeticSolver (Cons (Symbol "-") (Cons (SexprFloat f1) (Cons (Symbol s2) Nil))) env = let f2 = getval $ resolveVar env s2 in AGLfloat(f1-f2)
arithmeticSolver (Cons (Symbol "*") (Cons (SexprFloat f1) (Cons (Symbol s2) Nil))) env = let f2 = getval $ resolveVar env s2 in AGLfloat(f1*f2)
arithmeticSolver (Cons (Symbol "/") (Cons (SexprFloat f1) (Cons (Symbol s2) Nil))) env = let f2 = getval $ resolveVar env s2 in AGLfloat(f1/f2)

arithmeticSolver (Cons (Symbol "+") (Cons (Symbol s1) (Cons (SexprInt i2) Nil))) env = let i1 = getval $ resolveVar env s1 in AGLfloat(i1 + (fromIntegral i2))
arithmeticSolver (Cons (Symbol "-") (Cons (Symbol s1) (Cons (SexprInt i2) Nil))) env = let i1 = getval $ resolveVar env s1 in AGLfloat(i1 - (fromIntegral i2))
arithmeticSolver (Cons (Symbol "*") (Cons (Symbol s1) (Cons (SexprInt i2) Nil))) env = let i1 = getval $ resolveVar env s1 in AGLfloat(i1 * (fromIntegral i2))
arithmeticSolver (Cons (Symbol "/") (Cons (Symbol s1) (Cons (SexprInt i2) Nil))) env = let i1 = getval $ resolveVar env s1 in AGLfloat(i1 / (fromIntegral i2))
arithmeticSolver (Cons (Symbol "+") (Cons (Symbol s1) (Cons (SexprFloat f2) Nil))) env = let f1 = getval $ resolveVar env s1 in AGLfloat(f1+f2)
arithmeticSolver (Cons (Symbol "-") (Cons (Symbol s1) (Cons (SexprFloat f2) Nil))) env = let f1 = getval $ resolveVar env s1 in AGLfloat(f1-f2)
arithmeticSolver (Cons (Symbol "*") (Cons (Symbol s1) (Cons (SexprFloat f2) Nil))) env = let f1 = getval $ resolveVar env s1 in AGLfloat(f1*f2)
arithmeticSolver (Cons (Symbol "/") (Cons (Symbol s1) (Cons (SexprFloat f2) Nil))) env = let f1 = getval $ resolveVar env s1 in AGLfloat(f1/f2)


arithmeticSolver (Cons (Symbol "+") (Cons (Symbol s1) (Cons (Symbol s2) Nil))) env = AGLfloat(i1+i2)
  where i1 = getval $ resolveVar env s1
        i2 = getval $ resolveVar env s2
arithmeticSolver (Cons (Symbol "-") (Cons (Symbol s1) (Cons (Symbol s2) Nil))) env = AGLfloat(i1-i2)
  where i1 = getval $ resolveVar env s1
        i2 = getval $ resolveVar env s2
arithmeticSolver (Cons (Symbol "*") (Cons (Symbol s1) (Cons (Symbol s2) Nil))) env = AGLfloat(i1*i2)
  where i1 = getval $ resolveVar env s1
        i2 = getval $ resolveVar env s2
arithmeticSolver (Cons (Symbol "/") (Cons (Symbol s1) (Cons (Symbol s2) Nil))) env = AGLfloat(i1/i2)
  where i1 = getval $ resolveVar env s1
        i2 = getval $ resolveVar env s2





--TODO; same as above there probably is a more consise way to do this.
conditionalResolver :: Sexpr -> LocalEnv -> Bool

conditionalResolver (Cons (Symbol "<") (Cons (Symbol s1) (Cons (SexprFloat f2) Nil))) env = let f1 = getval $ resolveVar env s1 in f1 < f2
conditionalResolver (Cons (Symbol ">") (Cons (Symbol s1) (Cons (SexprFloat f2) Nil))) env = let f1 = getval $ resolveVar env s1 in f1 > f2
conditionalResolver (Cons (Symbol "=") (Cons (Symbol s1) (Cons (SexprFloat f2) Nil))) env = let f1 = getval $ resolveVar env s1 in f1 == f2

conditionalResolver (Cons (Symbol "<") (Cons (Symbol s1) (Cons (SexprInt i2) Nil))) env = let i1 = getval $ resolveVar env s1 in i1 < (fromIntegral i2)
conditionalResolver (Cons (Symbol ">") (Cons (Symbol s1) (Cons (SexprInt i2) Nil))) env = let i1 = getval $ resolveVar env s1 in i1 > (fromIntegral i2)
conditionalResolver (Cons (Symbol "=") (Cons (Symbol s1) (Cons (SexprInt i2) Nil))) env = let i1 = getval $ resolveVar env s1 in i1 == (fromIntegral i2)

conditionalResolver (Cons (Symbol "<") (Cons (SexprFloat f1) (Cons (Symbol s2) Nil))) env = let f2 = getval $ resolveVar env s2 in f1 < f2
conditionalResolver (Cons (Symbol ">") (Cons (SexprFloat f1) (Cons (Symbol s2) Nil))) env = let f2 = getval $ resolveVar env s2 in f1 > f2
conditionalResolver (Cons (Symbol "=") (Cons (SexprFloat f1) (Cons (Symbol s2) Nil))) env = let f2 = getval $ resolveVar env s2 in f1 == f2

conditionalResolver (Cons (Symbol "<") (Cons (SexprInt i1) (Cons (Symbol s2) Nil))) env = let i2 = getval $ resolveVar env s2 in (fromIntegral i1) < i2
conditionalResolver (Cons (Symbol ">") (Cons (SexprInt i1) (Cons (Symbol s2) Nil))) env = let i2 = getval $ resolveVar env s2 in (fromIntegral i1) > i2
conditionalResolver (Cons (Symbol "=") (Cons (SexprInt i1) (Cons (Symbol s2) Nil))) env = let i2 = getval $ resolveVar env s2 in (fromIntegral i1) == i2

conditionalResolver (Cons (Symbol "<") (Cons (Symbol s1) (Cons (Symbol s2) Nil))) env = f1 < f2
  where f1 = getval $ resolveVar env s1
        f2 = getval $ resolveVar env s2
conditionalResolver (Cons (Symbol ">") (Cons (Symbol s1) (Cons (Symbol s2) Nil))) env = f1 > f2
  where f1 = getval $ resolveVar env s1
        f2 = getval $ resolveVar env s2
conditionalResolver (Cons (Symbol "=") (Cons (Symbol s1) (Cons (Symbol s2) Nil))) env = f1 == f2
  where f1 = getval $ resolveVar env s1
        f2 = getval $ resolveVar env s2

graphicsTranslator :: GraphicsState -> GraphicsState
graphicsTranslator ([],instcpy,c,s,p,g,jt,env) = ([],instcpy,c,s,p,g,jt,env)
graphicsTranslator (((MyColor (Var var)):rest),instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,cnew,s,p,g,jt,env)
  where val = getval $ resolveVar env var
        cnew = hueToRGB val

graphicsTranslator (((MyColor (Arithmetic arithSexpr)):rest),instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,cnew,s,p,g,jt,env)
  where val = getval $ arithmeticSolver arithSexpr env
        cnew = hueToRGB val

graphicsTranslator (((MyColor val):rest),instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy, hueToRGB (getval val),s,p,g,jt,env)
graphicsTranslator (Penup:rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,"up",p,g,jt,env)
graphicsTranslator (Pendown:rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,"down",p,g,jt,env)

graphicsTranslator ((Make var (Arithmetic arithSexpr)):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,p,g,jt,updatedEnv)
  where val = arithmeticSolver arithSexpr env
        updatedEnv = updateEnv [(var,val)] env

graphicsTranslator ((Make var val):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,p,g,jt,updatedEnv)
  where updatedEnv = updateEnv [(var,val)] env

graphicsTranslator ((Forward (Var var)):rest,instcpy,c,s,p,g,jt,env) =  graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "F" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight val else Invisible val)]

graphicsTranslator ((Forward (Arithmetic arithSexpr)):rest,instcpy,c,s,p,g,jt,env) =  graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where val = getval $ arithmeticSolver arithSexpr env
        pnew = (updatePoint p "F" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight val else Invisible val)]

graphicsTranslator ((Forward val):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where pnew = (updatePoint p "F" ((getval val) :: Float))
        gnew = g ++ [Paint c $ (if s == "down" then Straight (getval val) else Invisible (getval val))]

graphicsTranslator ((Backward (Var var)):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "B" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight (-val) else Invisible (-val))]

graphicsTranslator ((Backward (Arithmetic arithSexpr)):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where val = getval $ arithmeticSolver arithSexpr env
        pnew = (updatePoint p "B" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight (-val) else Invisible (-val))]

graphicsTranslator ((Backward val):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where pnew = (updatePoint p "B" ((getval val) :: Float))
        gnew = g ++ [Paint c $ (if s == "down" then Straight (-(getval val)) else Invisible (-(getval val)))]

graphicsTranslator ((MyRight (Var var)):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "R" val)
        gnew = g ++ [Paint c $ Bend (-val)]

graphicsTranslator ((MyRight (Arithmetic arithSexpr)):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where val = getval $ arithmeticSolver arithSexpr env
        pnew = (updatePoint p "R" val)
        gnew = g ++ [Paint c $ Bend (-val)]

graphicsTranslator ((MyRight val):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env) 
  where pnew = (updatePoint p "R" ((getval val) :: Float))
        gnew = g ++ [Paint c $ Bend (-(getval val))]

graphicsTranslator ((MyLeft (Var var)):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "L" val)
        gnew = g ++ [Paint c $ Bend val]

graphicsTranslator ((MyLeft (Arithmetic arithSexpr)):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where val = getval $ arithmeticSolver arithSexpr env
        pnew = (updatePoint p "L" val)
        gnew = g ++ [Paint c $ Bend val]

graphicsTranslator ((MyLeft val):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where pnew = (updatePoint p "L" ((getval val) :: Float))
        gnew = g ++ [Paint c $ Bend (getval val)]

graphicsTranslator ((MyRepeat (Var var) inst):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,cnew,snew,pnew,gnew,jt,env)
  where  i = floor $ getval $ resolveVar env var
         (_,_,cnew,snew,pnew,gacc,_,_) = iterate (dup . graphicsTranslator . dup) (inst,inst,c,s,p,[],jt,env) !! i  -- Take the ith iteration. 
         gnew = g ++ gacc

graphicsTranslator ((MyRepeat (AInt i) inst):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,cnew,snew,pnew,gnew,jt,env)
  where (_,_,cnew,snew,pnew,gacc,_,_) = iterate (dup . graphicsTranslator . dup) (inst,inst,c,s,p,[],jt,env) !! i  -- Take the ith iteration. 
        gnew = g ++ gacc

graphicsTranslator ((SetXY a b):rest,instcpy,c,s,p@(x,y,oldang),g,jt,env) = graphicsTranslator (rest,instcpy,c,s,pnew,gnew,jt,env)
  where fltpnt = ((fromIntegral (getintval a)),(fromIntegral (getintval b)))
        ang = getAngle p fltpnt
        dist = getDist p fltpnt
        gnew = g ++ [(Bend $ ang), (if s == "down" then Straight dist else Invisible dist), (Bend $ -ang)]
        pnew = ((fromIntegral (getintval a)), (fromIntegral (getintval b)), oldang)

graphicsTranslator ((If (Conditional condsexpr) inst):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest, instcpy,cnew,snew,pnew,gnew,jt,env)
  where condResult = conditionalResolver condsexpr env
        (_,_,cnew,snew,pnew,gacc,_,_) = if condResult then graphicsTranslator (inst,[],c,s,p,[],jt,env) else ([],[],c,s,p,[],[],[])
        gnew = g ++ gacc

graphicsTranslator ((IfElse (Conditional condsexpr) ifinst elseinst):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest, instcpy,cnew,snew,pnew,gnew,jt,env)
  where condResult = conditionalResolver condsexpr env
        (_,_,cnew,snew,pnew,gacc,_,_) = if condResult then graphicsTranslator (ifinst,[],c,s,p,[],jt,env) else graphicsTranslator (elseinst,[],c,s,p,g,jt,env)
        gnew = g ++ gacc

graphicsTranslator ((Call funcName args):rest,instcpy,c,s,p,g,jt,env) = graphicsTranslator (rest,instcpy,cnew,snew,pnew,gnew,jt,env)
  where subProc = getSubRoute funcName jt
        params = getParams subProc
        subProcInst = getSubRouteInst subProc
        resolvedArgs = resolveArithArgs (resolveArgs args env) env
        bindings = zip params resolvedArgs
        subProcEnv = updateEnv bindings []
        (_,_,cnew,snew,pnew,gacc,_,_) = graphicsTranslator (subProcInst,[],c,s,p,[],jt,subProcEnv) 
        gnew = g ++ gacc


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
--testString = "(define foo '((repeat 3 (penup) (forward 5) (pendown) (forward 5))))"
 --testString = "(define foo '((repeat 4 (forward 5) (right 90)) (repeat 4 (forward 2) (right 90)))))"
--testString = "(define foo '((forward 10) (penup) (setxy 20 20) (pendown) (forward 10) (right 135) (forward 10)))"
-- testString = "(define foo '((+ 1 (+1 2))))"
--testString = "(define foo '((to testsub (arg) (forward arg)) (testsub 10)))"
--testString = "(define foo '((to testsub (arg1 arg2) (forward arg1) (right arg2) (forward 5) (right arg2) (forward arg1)) (testsub 10 90)))"
--testString = "(define foo '((to testsub (arg1 arg2 arg3) (forward arg1) (right arg2) (forward 5) (right arg2) (color arg3) (forward arg1)) (testsub 10 90 200)))"
--testString = "(define foo '((to testsub (arg1 arg2 arg3 arg4) (forward arg1) (right arg2) (color arg3) (forward arg1) (right arg2) (forward arg4)) (testsub 10 90 232 80)))"
--testString = "(define foo '((to square (side)(repeat 4 (forward side) (right 90))) (square 50) (square 25) (square 5))))"
-- testString = "(define foo '((to testsub (arg col) (make col 200) (color col) (forward arg)) (testsub 10 25)))"
--testString = "(define foo '((forward (* 2 2))))"
--testString = "(define foo '((to testsub (arg1 arg2 arg3) (forward (+ arg2 8)) (right 90) (forward (* arg2 arg1)) (right 90) (forward (* 2 5))) (testsub 1 2 3))))"
--testString = "(define foo '((to testsub (arg1 arg2 arg3 arg4) (color (+ arg2 arg3)) (forward (+ arg2 8)) (right (* arg3 2)) (backward (* arg2 arg1)) (left (- 3 5)) (forward (* arg4 arg4))) (testsub 1 2 3 4))))"
--testString = "(define foo '((to testsub (arg1) (if (> arg1 1) ((color 200) (forward 3))) (forward 10)) (testsub 3)))"
-- testString = "(define circles'((to circle (seg clr)(if (< seg 1)(forward 0)(repeat 5(repeat 8(make clr (+ clr 10))(forward seg)(right 9))(right 180)(circle (/ seg 2) (+ clr 47))(right 180))))(penup)(setxy -50 200)(pendown)(circle 10 0)))"
-- testString = "(define koch'((to koch (n) (if (= n 1) (forward 8) ((koch (- n 1)) (left 60) (koch (- n 1)) (right 120) (koch (- n 1)) (left 60) (koch (- n 1)))))(repeat 3 (koch 4)(right 120))))"
-- testString = "(define foo'((to bar (x)(if (> x 0)((forward x)(right 90)(bar (- x 4)))))(bar 40)))"
testString = "(define hilbert'((to hilbert (size level parity)(if (> level 0)((left (* parity 90))(hilbert size (- level 1) (- parity))(forward size)(right (* parity 90))(hilbert size (- level 1) parity)(forward size)(hilbert size (- level 1) parity)(right (* parity 90))(forward size)(hilbert size (- level 1) (- parity))(left (* parity 90)))))(hilbert 10 4 1)))"
--testString = "(define hilbert'((to hilbert (size level parity)(if (> level 0) ((left (* parity 90))(hilbert size (- level 1) (- parity))(forward size)(right (* parity 90))(hilbert size (- level 1) parity)(forward size)(hilbert size (- level 1) parity))))(hilbert 10 2 1)))"

--testString = "(define foo '((to testsub (a b c) (if (> b 1) ((testsub 1 (- b 1) (- c 1)) (forward c) (right 5) (testsub 1 (- b 1) (- c 1))))) (testsub 1 5 9)))"

-- TODO Below does not work because of bug in replicate that needs to be fixed, try iterate?
--testString = "(define starfish '((to starfish (side angle inc) (repeat 90 (forward side) (right angle) (make angle (+ angle inc)))) (penup) (forward 50) (pendown) (starfish 30 2 20)))))"
--testString = "(define stars'((to stars (side angle max) (repeat 5 (star side angle max 1))) (to star (side angle max count) (repeat max (forward (* side count)) (right angle) (make count (+ count 1))))(penup)(forward 50)(pendown)(stars 15 144 8)(penup)(backward 50)))"
--testString = "(define foo '((repeat 5(forward 50)(right (/ 360 5)))))"
--testString = "(define foo '((to spiral(side angle max count)(repeat max(forward (* side count))(right angle)(make count (+ count 1))))(penup)(forward 70)(pendown)(spiral 0.05 10 180 0)))"

--testString = "(define foo '((if (= 1 n) ((forward 10) (penup)))))"
--testString = "(define circles'((to circle (seg clr)(if (< seg 1)(forward 0)(repeat 5 (repeat 8 (make clr (+ clr 10))(forward seg)(right 9))(right 180)(circle (/ seg 2) (+ clr 47))(right 180))))(penup)(setxy -50 200)(pendown)(circle 10 0)))"

(debugGetInstStream, debugJt) = (preprocessor (stripHeader $ p testString) ([],[]))

debugGetGraphicsInstStream = ([Bend 90] ++ (getGraphicInstStream (graphicsTranslator (debugGetInstStream,[],white,"down",(0.0,0.0,0.0),[],debugJt,[]))))
    where (debugGetInstStream, debugJt) = (preprocessor (stripHeader $ p testString) ([],[]))


main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Haskell Plumbing Graphics"
  let (instructionStream, jt) = (preprocessor (stripHeader $ p testString) ([],[]))
  let graphicsInstructionStream = ([Bend 90] ++ (getGraphicInstStream (graphicsTranslator (instructionStream,[], white,"down",(0.0,0.0,0.0),[],jt,[]))))
  wrappedInstStream <- newIORef graphicsInstructionStream
  displayCallback $= display wrappedInstStream
  actionOnWindowClose $= MainLoopReturns
  mainLoop

display is = do
  clear [ColorBuffer]
  graphicInstStream <- readIORef is
  putStrLn $ show $ length graphicInstStream
  draw $ (myJoin graphicInstStream)
  flush


-- Stages now: Sexpr -> [Instruction] -> [Graphic]
-- We can't jump from Sexpr -> [Graphic] or we can't maintain state.
-- We have to use [Graphic] because it's how we can interface with Lances library.
