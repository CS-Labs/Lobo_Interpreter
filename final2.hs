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

data Sexpr = Symbol String | SexprInt {getsint :: Int} | SexprFloat {getsfloat :: Float} | Nil | Cons {car :: Sexpr, cdr :: Sexpr} 


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

type TS = String -- Translator state. 


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


type PenState = (String,ColorTriple,(Float,Float,Float))

type GraphicsState =  ([Instruction], [Instruction], PenState, [PenState], [Graphic], JumpTable, LocalEnv, TS)


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
updatePoint (x,y,a) "F" n = (x+(n*cos(degToRad a)),(y+(n*sin(degToRad a))), a)
updatePoint (x,y,a) "B" n = (x+(n*cos(degToRad a)),(y+(n*sin(degToRad a))),a)
updatePoint (x,y,a) "R" n = (x,y,a-n)
updatePoint (x,y,a) "L" n = (x,y,a+n)



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
resolveArithArgs args env = [if isArith arg then (AGLfloat $ arithmeticSolver (getsexpr arg) env) else arg | arg <- args]

-- -- To call function pass: zip vars values
updateEnv :: [(String, Data)] -> LocalEnv -> LocalEnv
updateEnv [] env = env
updateEnv bindings@((var, val):xs) env = updateEnv xs (updateEnvHelper var val env)
  
updateEnvHelper :: String -> Data -> LocalEnv -> LocalEnv
updateEnvHelper var val env = if var `elem` [s | (s,v) <- env]
  then map (\(s,v) -> if s == var then (s, val) else (s,v)) env
  else [(var,val)] ++ env

isOn :: String -> Bool
isOn "on" = True
isOn "off" = False

getAngle :: (Float, Float, Float) -> (Float, Float) -> Float
getAngle (x0,y0,a) (x,y) = (radToDeg $ (atan2 (y0-y) (x0-x))) + 90.0 + (90.0 - a)


getDist :: (Float, Float, Float) -> (Float, Float) -> Float
getDist (x0,y0,a) (x,y) = (sqrt((x-x0)^2 + (y-y0)^2))

stripHeader :: Sexpr -> Sexpr
stripHeader (Cons (Symbol "define") (Cons (Symbol s) (Cons (sexpr) Nil))) = sexpr

stripJumpTable :: ([Instruction], JumpTable) -> [Instruction]
stripJumpTable (inst,jt) = inst

setxyHelper :: Sexpr -> String -> Data
setxyHelper s "i" = (AGLfloat (fromIntegral $ getsint s))
setxyHelper s "f" = (AGLfloat (getsfloat s))
setxyHelper s "a" = getexpr $ head (stripJumpTable $ preprocessor (Cons s Nil) ([],[]))

myt = "((setxy (* (cos (+ (* t a) c)) 75) (+ (* (sin (* t b)) 75) 100)))"

mytest (Cons (Cons (Symbol "setxy") arithSexpr) rest)  = inst
  where first = car arithSexpr
        second = car $ cdr arithSexpr
        temp1 = if elem ')' (show first) then "a" else "i"
        temp2 = if elem ')' (show second) then "a" else "i"
        type1 = if elem '.' (show first) && temp1 == "i" then "f" else temp1
        type2 = if elem '.' (show second) && temp2 == "i" then "f" else temp2
        inst = [SetXY (setxyHelper (Cons first Nil) type1) (setxyHelper (Cons second Nil) type2)]



preprocessor :: Sexpr -> ([Instruction], JumpTable) -> ([Instruction], JumpTable)
preprocessor (Nil) (instStream, jt) = (instStream, jt)
preprocessor (Cons (SexprInt val) Nil) (instStream, jt) = ((instStream ++ [NoOp (AGLfloat (fromIntegral val))]), jt)
preprocessor (Cons (SexprFloat val) Nil) (instStream, jt) = ((instStream ++ [NoOp (AGLfloat val)]), jt)
preprocessor (Cons (Symbol val) Nil) (instStream, jt) = ((instStream ++ [NoOp (Var val)], jt))
preprocessor (Cons (SexprInt val) rest) (instStream, jt) = preprocessor rest ((instStream ++ [NoOp (AGLfloat (fromIntegral val))]), jt)
preprocessor (Cons (SexprFloat val) rest) (instStream, jt) = preprocessor rest ((instStream ++ [NoOp (AGLfloat val)]), jt)
preprocessor (Cons (Cons (Symbol "penup") Nil) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Penup]),jt)
preprocessor (Cons (Cons (Symbol "pendown") Nil) rest) (instStream, jt)  = preprocessor rest ((instStream ++ [Pendown]),jt)
preprocessor (Cons (Cons (Symbol "stop") Nil) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Stop]),jt)
preprocessor (Cons (Cons (Symbol "push") Nil) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Push]),jt)
preprocessor (Cons (Cons (Symbol "pop") Nil) rest) (instStream, jt) = preprocessor rest ((instStream ++ [Pop]),jt)
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
preprocessor (Cons (Cons (Symbol "setxy") arithSexpr) rest) (instStream, jt) = preprocessor rest ((instStream ++ inst),jt)
  where first = car arithSexpr
        second = car $ cdr arithSexpr
        temp1 = if elem ')' (show first) then "a" else "i"
        temp2 = if elem ')' (show second) then "a" else "i"
        type1 = if elem '.' (show first) && temp1 == "i" then "f" else temp1
        type2 = if elem '.' (show second) && temp2 == "i" then "f" else temp2
        inst = [SetXY (setxyHelper first type1) (setxyHelper second type2)]

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

-- Params to functions. 
preprocessor (Cons (Symbol val) rest) (instStream, jt) = preprocessor rest ((instStream ++ [NoOp (Var val)]), jt)

-- Triple nested cons are reduced by one level.
preprocessor (Cons rest nil) (instStream, jt) = preprocessor rest (instStream,jt)  

--type GraphicsState =  ([Instruction], [Instruction], PenState, [Graphic], JumpTable, LocalEnv, TS)

getGraphicInstStream :: GraphicsState-> [Graphic]
getGraphicInstStream (_,_,_,_,graphicInstStream,_,_,_) = graphicInstStream

dup :: ([Instruction], [Instruction], PenState, [PenState], [Graphic], JumpTable, LocalEnv, TS) -> ([Instruction], [Instruction], PenState,[PenState], [Graphic], JumpTable, LocalEnv,TS)
dup (a,b,c,d,e,f,g,h) = (b,b,c,d,e,f,g,h)

arithmeticSolver (Cons (Symbol "+") (Cons s1 (Cons s2 Nil))) env = (+) (arithmeticSolver s1 env) (arithmeticSolver s2 env) 
arithmeticSolver (Cons (Symbol "*") (Cons s1 (Cons s2 Nil))) env = (*) (arithmeticSolver s1 env) (arithmeticSolver s2 env) 
arithmeticSolver (Cons (Symbol "-") (Cons s1 (Cons s2 Nil))) env = (-) (arithmeticSolver s1 env) (arithmeticSolver s2 env) 
arithmeticSolver (Cons (Symbol "/") (Cons s1 (Cons s2 Nil))) env = (/) (arithmeticSolver s1 env) (arithmeticSolver s2 env)
arithmeticSolver (Cons (Symbol "sin") (Cons s Nil)) env = sin (arithmeticSolver s env)
arithmeticSolver (Cons (Symbol "cos") (Cons s Nil)) env = cos (arithmeticSolver s env)
arithmeticSolver (Cons (Symbol "sqrt") (Cons s Nil)) env = sqrt (arithmeticSolver s env)
arithmeticSolver (SexprInt n) env = fromIntegral n
arithmeticSolver (SexprFloat n) env = n
arithmeticSolver (Symbol s) env = getval $ resolveVar env s

conditionalResolver :: Sexpr -> LocalEnv -> Bool
conditionalResolver (Cons (Symbol "<") (Cons s1 (Cons s2 Nil))) env = (<) (condHelper s1 env) (condHelper s2 env)
conditionalResolver (Cons (Symbol ">") (Cons s1 (Cons s2 Nil))) env = (>) (condHelper s1 env) (condHelper s2 env)
conditionalResolver (Cons (Symbol "=") (Cons s1 (Cons s2 Nil))) env = (==) (condHelper s1 env) (condHelper s2 env)
conditionalResolver (Cons (Symbol "<=") (Cons s1 (Cons s2 Nil))) env = (<=) (condHelper s1 env) (condHelper s2 env)
conditionalResolver (Cons (Symbol ">=") (Cons s1 (Cons s2 Nil))) env = (>=) (condHelper s1 env) (condHelper s2 env)

condHelper (SexprInt n) env = fromIntegral n
condHelper (SexprFloat n) env = n
condHelper (Symbol s) env = getval $ resolveVar env s


graphicsTranslator :: GraphicsState -> GraphicsState
graphicsTranslator ([],instcpy,(s,c,p),stack,g,jt,env,vs) = ([],instcpy,(s,c,p),stack,g,jt,env,vs)
graphicsTranslator ((Stop:rest),instcpy,(s,c,p),stack,g,jt,env,vs) = ([],instcpy,(s,c,p),stack,g,jt,env,"off")
graphicsTranslator ((Push:rest),instcpy,(s,c,p),stack,g,jt,env,vs) = graphicsTranslator (rest,instcpy,(s,c,p),(s,c,p):stack,g,jt,env,vs)
graphicsTranslator ((Pop:rest),instcpy,(s,c,p),stack,g,jt,env,vs) = graphicsTranslator (rest,instcpy,newpstate,newstack,gnew,jt,env,vs)
  where oldpstate@(sold,cold,pold@(xold,yold,oldang)) = (s,c,p)
        newpstate@(snew,cnew,pnew@(xnew,ynew,newang)) = head stack
        newstack = tail stack
        ang = getAngle pold (xnew,ynew)
        dist = getDist pold (xnew,ynew)
        gtmp = (if xnew == xold && ynew == xold then [] else [(Paint c $ Bend $ ang), Paint c $ Invisible dist, (Paint c $ Bend $ -ang)]) ++ [(Paint c $ Bend $ (newang - oldang))]
        gnew = g ++ gtmp

graphicsTranslator (((MyColor (Var var)):rest),instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,cnew,p),stack,g,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ resolveVar env var
        cnew = hueToRGB val

graphicsTranslator (((MyColor (Arithmetic arithSexpr)):rest),instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,cnew,p),stack,g,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
        cnew = hueToRGB val

graphicsTranslator (((MyColor val):rest),instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,cnew,p),stack,g,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where cnew = hueToRGB (getval val)

graphicsTranslator (Penup:rest,instcpy,(s,c,p),stack,g,jt,env,vs) = graphicsTranslator (rest,instcpy,("up",c,p),stack,g,jt,env,vs)

graphicsTranslator (Pendown:rest,instcpy,(s,c,p),stack,g,jt,env,vs) = graphicsTranslator (rest,instcpy,("down",c,p),stack,g,jt,env,vs)

graphicsTranslator ((Make var (Arithmetic arithSexpr)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,p),stack,g,jt,updatedEnv,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = (AGLfloat $ arithmeticSolver arithSexpr env)
        updatedEnv = updateEnv [(var,val)] env

graphicsTranslator ((Make var val):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,p),stack,g,jt,updatedEnv,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where updatedEnv = updateEnv [(var,val)] env

graphicsTranslator ((Forward (Var var)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) =  
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "F" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight val else Invisible val)]

graphicsTranslator ((Forward (Arithmetic arithSexpr)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) =  
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
        pnew = (updatePoint p "F" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight val else Invisible val)]

graphicsTranslator ((Forward val):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where pnew = (updatePoint p "F" ((getval val) :: Float))
        gnew = g ++ [Paint c $ (if s == "down" then Straight (getval val) else Invisible (getval val))]

graphicsTranslator ((Backward (Var var)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "B" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight (-val) else Invisible (-val))]

graphicsTranslator ((Backward (Arithmetic arithSexpr)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
        pnew = (updatePoint p "B" val)
        gnew = g ++ [Paint c $ (if s == "down" then Straight (-val) else Invisible (-val))]

graphicsTranslator ((Backward val):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where pnew = (updatePoint p "B" ((getval val) :: Float))
        gnew = g ++ [Paint c $ (if s == "down" then Straight (-(getval val)) else Invisible (-(getval val)))]

graphicsTranslator ((MyRight (Var var)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "R" val)
        gnew = g ++ [Paint c $ Bend (-val)]

graphicsTranslator ((MyRight (Arithmetic arithSexpr)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,pnew),stack,g,jt,env,vs)
  where val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
        pnew = (updatePoint p "R" val)
        gnew = g ++ [Paint c $ Bend (-val)]

graphicsTranslator ((MyRight val):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs)  else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where pnew = (updatePoint p "R" ((getval val) :: Float))
        gnew = g ++ [Paint c $ Bend (-(getval val))]

graphicsTranslator ((MyLeft (Var var)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([], instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ resolveVar env var
        pnew = (updatePoint p "L" val)
        gnew = g ++ [Paint c $ Bend val]

graphicsTranslator ((MyLeft (Arithmetic arithSexpr)):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
        pnew = (updatePoint p "L" val)
        gnew = g ++ [Paint c $ Bend val]

graphicsTranslator ((MyLeft val):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where pnew = (updatePoint p "L" ((getval val) :: Float))
        gnew = g ++ [Paint c $ Bend (getval val)]

graphicsTranslator ((MyRepeat (Var var) inst):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where  i = floor $ getval $ resolveVar env var
         (_,_,(snew,cnew,pnew),stacknew,gacc,_,_,_) = iterate (dup . graphicsTranslator . dup) (inst,inst,(s,c,p),stack,[],jt,env,vs) !! i  -- Take the ith iteration. 
         gnew = g ++ gacc

graphicsTranslator ((MyRepeat (AInt i) inst):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where (_,_,(snew,cnew,pnew),stacknew,gacc,_,_,_) = iterate (dup . graphicsTranslator . dup) (inst,inst,(s,c,p),stack,[],jt,env,vs) !! i  -- Take the ith iteration. 
        gnew = g ++ gacc

graphicsTranslator ((SetXY a b):rest,instcpy,(s,c,p@(x,y,oldang)),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(s,c,pnew),stack,gnew,jt,env,vs) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where xnew = getval $ (if isArith a then AGLfloat $ (arithmeticSolver (getsexpr a) env) else a)
        ynew = getval $ (if isArith b then AGLfloat $ (arithmeticSolver (getsexpr b) env) else b)
        fltpnt = (xnew, ynew)
        ang = getAngle p fltpnt
        dist = getDist p fltpnt
        gnew = g ++ (if xnew == x && ynew == y then [] else [(Paint c $ Bend $ ang), Paint c $ (if s == "down" then Straight dist else Invisible dist), (Paint c $ Bend $ -ang)])
        pnew = (xnew, ynew, oldang)

graphicsTranslator ((If (Conditional condsexpr) inst):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest, instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vsnew) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where condResult = conditionalResolver condsexpr env
        (_,_,(snew,cnew,pnew),stacknew,gacc,_,_,vsnew) = if condResult then graphicsTranslator (inst,[],(s,c,p),stack,[],jt,env,vs) else ([],[],(s,c,p),stack,[],[],[],vs)
        gnew = g ++ gacc

graphicsTranslator ((IfElse (Conditional condsexpr) ifinst elseinst):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest, instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vsnew) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where condResult = conditionalResolver condsexpr env
        (_,_,(snew,cnew,pnew),stacknew,gacc,_,_,vsnew) = if condResult then graphicsTranslator (ifinst,[],(s,c,p),stack,[],jt,env,vs) else graphicsTranslator (elseinst,[],(s,c,p),stack,g,jt,env,vs)
        gnew = g ++ gacc

graphicsTranslator ((Call funcName args):rest,instcpy,(s,c,p),stack,g,jt,env,vs) = 
  if isOn vs then graphicsTranslator (rest,instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vsnew) else ([],instcpy,(s,c,p),stack,g,jt,env,vs)
  where subProc = getSubRoute funcName jt
        params = getParams subProc
        subProcInst = getSubRouteInst subProc
        resolvedArgs = resolveArithArgs (resolveArgs args env) env
        bindings = zip params resolvedArgs
        subProcEnv = updateEnv bindings []
        (_,_,(snew,cnew,pnew),stacknew,gacc,_,_,vsnew) = graphicsTranslator (subProcInst,[],(s,c,p),stack,[],jt,subProcEnv,vs) 
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
--testString = "(define circles'((to circle (seg clr)(if (< seg 1)(forward 0)(repeat 5(repeat 8(make clr (+ clr 10))(forward seg)(right 9))(right 180)(circle (/ seg 2) (+ clr 47))(right 180))))(penup)(setxy -50 200)(pendown)(circle 10 0)))"
--testString = "(define koch'((to koch (n) (if (= n 1) (forward 8) ((koch (- n 1)) (left 60) (koch (- n 1)) (right 120) (koch (- n 1)) (left 60) (koch (- n 1)))))(repeat 3 (koch 4)(right 120))))"
-- testString = "(define foo'((to bar (x)(if (> x 0)((forward x)(right 90)(bar (- x 4)))))(bar 40)))"
--testString = "(define hilbert'((to hilbert (size level parity)(if (> level 0)((left (* parity 90))(hilbert size (- level 1) (- parity))(forward size)(right (* parity 90))(hilbert size (- level 1) parity)(forward size)(hilbert size (- level 1) parity)(right (* parity 90))(forward size)(hilbert size (- level 1) (- parity))(left (* parity 90)))))(hilbert 10 4 1)))"
--testString = "(define hilbert'((to hilbert (size level parity)(if (> level 0) ((left (* parity 90))(hilbert size (- level 1) (- parity))(forward size)(right (* parity 90))(hilbert size (- level 1) parity)(forward size)(hilbert size (- level 1) parity))))(hilbert 10 2 1)))"

--testString = "(define foo '((to testsub (a b c) (if (> b 1) ((testsub 1 (- b 1) (- c 1)) (forward c) (right 5) (testsub 1 (- b 1) (- c 1))))) (testsub 1 5 9)))"

--testString = "(define starfish '((to starfish (side angle inc) (repeat 90 (forward side) (right angle) (make angle (+ angle inc)))) (penup) (forward 50) (pendown) (starfish 30 2 20)))))"
--testString = "(define stars'((to stars (side angle max) (repeat 5 (star side angle max 1))) (to star (side angle max count) (repeat max (forward (* side count)) (right angle) (make count (+ count 1))))(penup)(forward 50)(pendown)(stars 15 144 8)(penup)(backward 50)))"
-- testString = "(define foo '((repeat 5(forward 50) (stop) (right (/ 360 5)))))"
--testString = "(define foo '((to spiral(side angle max count)(repeat max(forward (* side count))(right angle)(make count (+ count 1))))(penup)(forward 70)(pendown)(spiral 0.05 10 180 0)))"

--testString = "(define foo '((if (= 1 n) ((forward 10) (penup)))))"
--testString = "(define circles'((to circle (seg clr)(if (< seg 1)(forward 0)(repeat 5 (repeat 8 (make clr (+ clr 10))(forward seg)(right 9))(right 180)(circle (/ seg 2) (+ clr 47))(right 180))))(penup)(setxy -50 200)(pendown)(circle 10 0)))"

--testString = "(define foo '((setxy 0 0) (setxy 2 0) (setxy 9 2) (setxy 5 9) (setxy 5 3)   ) )"

--testString = "(define foo '((to testsub (x y) (make x (+ 1 x)) (make y (+ 1 y)) (setxy (+ 1 x) (+ 1 y)) (setxy (- x 1) (- y 1))  ) (testsub 1 2)))"

--testString = "(define foo '((to testsub (x y) (setxy 50 50) (setxy 50 50) (setxy (* (cos 1) 50) (+ (* (sin 1) 50) 50))) (testsub 1 2)))"


--testString = "(define foo '((to circle (h r)(repeat 3 (setxy (+ h 1) (+ r 1)) (make h (+ h 1)) (make r (+ r 2))))(circle 0 0)))"

--testString = "(define foo '((to circle (h r)(setxy (+ h 1) (+ r 1)) (make h (+ h 1)) (make r (+ r 2))(setxy (+ h 1) (+ r 1)) (make h (+ h 1)) (make r (+ r 2))(setxy (+ h 1) (+ r 1)) (make h (+ h 1)) (make r (+ r 2)) ) (circle 0 0)))"

--testString = "(define foo '((to testsub (a b) (setxy (+ a 1) (+ b 1)) (setxy (+ a 1) (+ a 1)) (setxy (+ b 1) (+ b 1)))(testsub 0 3)   ))"

--testString = "(define foo '((setxy 1 4) (setxy 1 1) (setxy 4 4)))"
--testString = "(define broccoli'((to broccoli (x y)    (penup)    (left 90)    (forward 50)    (right 90)    (pendown)    (broccoli1 x y)  )  (to broccoli1 (x y)    (if (< x y)      (stop)      ((square x)       (forward x)       (left 45)       (broccoli1 (/ x (sqrt 2)) y)       (penup)       (backward (/ x (sqrt 2)))       (left 45)       (pendown)       (backward x)      )    )  )  (to square (x)    (repeat 4      (forward x)      (right 90)    )  )  (broccoli 100 1)))"
--testString = "(define fancy-spiral'((to fancy-spiral (size angle)(if (> size 200)(stop))(color (* size (/ 360 200)))(forward size)(right angle)(fancy-spiral (+ size 1) angle))(penup)(forward 120)(pendown)(fancy-spiral 0 91)))"
--testString = "(define foo '((to circle (h r)   (repeat 90     (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4))   )  ) (penup) (setxy 50 50) (pendown) (circle 0 0))))"
--testString = "(define lissajous '((to lissajous (a b c t)(penup)(setxy (* (cos c) 75) 100)(pendown)(repeat 364 (color t)(setxy (* (cos (+ (* t a) c)) 75) (+ (* (sin (* t b)) 75) 100))(make t (+ t 1))))(lissajous 0.1396 -0.12215 0.2094 0)))"
-- testString = "(define foo '((to circle (h r)   (repeat 12     (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4))     (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4))     (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4))     (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4)) (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4))     (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4))     (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4))     (color h)     (make r (* (/ h 360) (* 2 3.1416)))     (setxy (* (cos r) 50) (+ (* (sin r) 50) 50))     (make h (+ h 4))  )  ) (penup) (setxy 50 50) (pendown) (circle 0 0))))"
-- testString = "(define foo '((to circle (h r)(repeat 1 (setxy 50.0 50.0) (color 0)(setxy 49.878201943590916 53.48783183000035) (color 4)(setxy 49.513401165025456 56.958671214481676) (color 8)(setxy 48.90737494533586 60.39560849379845) (color 12)(setxy 48.063075797149764 63.781899176725844) (color 16)(setxy 46.984617080278255 67.10104551831918) (color 20)(setxy 45.677252961746625 70.33687689564475) (color 24)(setxy 44.147352817901115 73.4736285897975) (color 28)(setxy 42.40237020330254 76.496018590386) (color 32)(setxy 40.450806537587944 79.38932204829898) (color 36)(setxy 38.30216968721501 82.13944301402542) (color 40)(setxy 35.96692764387593 84.73298311202471) (color 44)(setxy 33.45645752525564 87.15730681657062) (color 48)(setxy 30.782990146598966 89.40060301105036) (color 52)(setxy 27.959550433129664 91.45194253080643) (color 56)(setxy 24.999893963627283 93.30133140917715) (color 60)(setxy 21.918439954316774 94.93975956732535) (color 64)(setxy 18.73020100956807 96.35924471064186) (color 68)(setxy 15.450709981654692 97.55287121786439) (color 72)(setxy 12.095944295905072 98.51482383344663) (color 76)(setxy 8.6822481099285 99.24041599903114) (color 80)(setxy 5.226252686149888 99.7261126859974) (color 84)(setxy 1.7447953655922528 99.96954761784629) (color 88)(setxy -1.745162462339811 99.96953479851538) (color 92)(setxy -5.226617994430631 99.72607429045947) (color 96)(setxy -8.682609849988886 99.24035221434627) (color 100)(setxy -12.096300705375636 98.51473497036883) (color 104)(setxy -15.451059324136226 97.55275770932768) (color 108)(setxy -18.730541583091153 96.35910710965099) (color 112)(setxy -21.918770099633697 94.93959854426166) (color 116)(setxy -25.00021207229568 93.3011477485325) (color 120)(setxy -27.959854955348863 91.45173712736118) (color 124)(setxy -30.783279598760405 89.40037686551413) (color 128)(setxy -33.45673049717085 87.1570610307069) (color 132)(setxy -35.967182805647006 84.7327188832831) (color 136)(setxy -38.30240579571363 82.13916162970781) (color 140)(setxy -40.45102244251184 79.3890248792879) (color 144)(setxy -42.402564852779726 76.49570708446576) (color 148)(setxy -44.14752526361446 73.47330426460073) (color 152)(setxy -45.67740236355403 70.33654133125859) (color 156)(setxy -46.984742710305724 67.10070034958726) (color 160)(setxy -48.06317704333756 63.78154608528335) (color 164)(setxy -48.90745131442044 60.39524919987944) (color 168)(setxy -49.51345228494258 56.95830746853821) (color 172)(setxy -49.87822756528831 53.48746540417439) (color 176)(setxy -49.99999999865076 49.999632679489665) (color 180)(setxy -49.8781763192016 46.51180174436192) (color 184)(setxy -49.5133500424361 43.0409650399504) (color 188)(setxy -48.907298573611754 39.604032212843606) (color 192)(setxy -48.06297454836802 36.217747732575475) (color 196)(setxy -46.98449144771504 32.8986093138718) (color 200)(setxy -45.677103557474034 29.66278754106668) (color 204)(setxy -44.147180369805156 26.526047086272612) (color 208)(setxy -42.402175551536914 23.503669905123743) (color 212)(setxy -40.45059063048092 20.61038078427607) (color 216)(setxy -38.30193357664922 17.860275603391514) (color 220)(setxy -35.96667248016372 15.266752661108214) (color 224)(setxy -33.45618455153477 12.842447399571022) (color 228)(setxy -30.782700692776153 10.599170845539831) (color 232)(setxy -27.959245909401492 8.547852067985481) (color 236)(setxy -24.99957585360965 6.69848493251515) (color 240)(setxy -21.91810980781692 5.060079412036345) (color 244)(setxy -18.729860435034144 3.640617690869277) (color 248)(setxy -15.450360638339308 2.4470152761653097) (color 252)(setxy -12.09558788578166 1.485087306093888) (color 256)(setxy -8.681886369399514 0.7595202189414607) (color 260)(setxy -5.225887377587117 0.2738489211483852) (color 264)(setxy -1.7444282687505177 0.030439565519650102) (color 268)(setxy 1.7455295589931938 0.030478023512358732) (color 272)(setxy 5.226983302429306 0.2739641077621613) (color 276)(setxy 8.682971589580674 0.759711572996089) (color 280)(setxy 12.096657114193391 1.4853538953272931) (color 284)(setxy 15.451408665783873 2.447355801775444) (color 288)(setxy 18.730882155603354 3.6410304938418605) (color 292)(setxy 21.91910024376763 5.060562481227407) (color 296)(setxy 25.000530179614834 6.699035914449112) (color 300)(setxy 27.960159476059086 8.54846827832121) (color 304)(setxy 30.783569049260457 10.599849282148504) (color 308)(setxy 33.45700346728046 12.843184757162206) (color 312)(setxy 35.96743796547693 15.267545347333012) (color 316)(setxy 38.302641902145055 17.861119756344316) (color 320)(setxy 40.45123834525263 20.61127229130932) (color 324)(setxy 42.40275949996845 23.504604422884423) (color 328)(setxy 44.14769770694519 26.527020061862906) (color 332)(setxy 45.67755176289624 29.663794234225136) (color 336)(setxy 46.98486833779744 32.899644820067635) (color 340)(setxy 48.063278286931414 36.218807006902956) (color 344)(setxy 48.907527680865506 39.6051100946006) (color 348)(setxy 49.51350340218747 43.042056277780794) (color 352)(setxy 49.8782531842938 46.5129010218398) (color 356)))(penup)(setxy 50 50) (pendown)(circle 0 0)))"

--testString = "(define tree'((to tree (depth count)(forward (* depth 20))(right 90)(if (> depth 1)(repeat 5 (push)(left (* count 30))(color (* 60 count)) (tree (- depth 1) 1)(pop)(make count (+ count 1)))))(tree 4 1)))"
--testString = "(define foo '(((color 200)(forward 25)(push)(color 0)(right 45)(forward 50)(right 45)(forward 20)(pop)(forward 25))))"

testString = "(define hexfield '((to hexfield (n c)(if (= n 1)(repeat 6 (forward 20) (left 60)(color (* c 60))(make c (+ c 1))) (repeat 6 (forward 20) (push) (right 180) (hexfield (- n 1) 0)(pop)(left 60))))(penup)(forward 100)(pendown)(right 90)(hexfield 3 0)))"

(debugGetInstStream, debugJt) = (preprocessor (stripHeader $ p testString) ([],[]))

debugGetGraphicsInstStream = ([Bend 90] ++ (getGraphicInstStream (graphicsTranslator (debugGetInstStream,[],("down",white,(0.0,0.0,90.0)),[("down",white,(0.0,0.0,90.0))],[],debugJt,[],"on"))))
    where (debugGetInstStream, debugJt) = (preprocessor (stripHeader $ p testString) ([],[]))


main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Haskell Plumbing Graphics"
  let (instructionStream, jt) = (preprocessor (stripHeader $ p testString) ([],[]))
  let graphicsInstructionStream = ([Bend 90] ++ (getGraphicInstStream (graphicsTranslator (instructionStream,[], ("down",white,(0.0,0.0,90.0)),[("down",white,(0.0,0.0,90.0))],[],jt,[],"on"))))
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
