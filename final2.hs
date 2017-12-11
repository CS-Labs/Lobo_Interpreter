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

type GraphicsState =  ([Instruction], PenState, [PenState], [Graphic], JumpTable, LocalEnv, TS)
type GSM a = StateT GraphicsState IO a

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


type ProgState a = StateT ([Instruction], JumpTable) IO a


elseHelper :: Sexpr -> ProgState ([Instruction])
elseHelper s = do
  (inst,jt) <- get -- Needed? Taking one to many elements?
  if isNil s then do
    return []
  else if (not $ isNil s) then do
    put([],jt)
    preprocessor (Cons s Nil)
    (ifelseInst,_) <- get
    return (ifelseInst)
  else do
    return ([])

ifHelper :: Sexpr -> ProgState ([Instruction])
ifHelper s = do
  (inst,jt) <- get -- Needed? Taking one to many elements?
  put([],[])
  preprocessor (Cons s Nil)
  (ifinst,_) <- get
  return (ifinst)

setXYHelper :: Sexpr -> String -> ProgState (Data)
setXYHelper s t = do
  (inst,jt) <- get -- Needed? Taking one to many elements?
  if t == "i" then do
    return ((AGLfloat (fromIntegral $ getsint s)))
  else if t == "f" then do
    return ((AGLfloat (getsfloat s)))
  else do 
    put([],jt)
    preprocessor (Cons s Nil)
    (instTmp,_) <- get
    return (getexpr $ head $ instTmp)

preprocessor :: Sexpr -> ProgState ()
preprocessor (Nil) = return ()
preprocessor (Cons (SexprInt val) Nil) = do {(inst,jt) <- get; put (inst ++ [NoOp (AGLfloat (fromIntegral val))],jt);}
preprocessor (Cons (SexprFloat val) Nil) = do {(inst,jt) <- get; put (inst ++ [NoOp (AGLfloat val)], jt);}
preprocessor (Cons (Symbol val) Nil) = do {(inst,jt) <- get; put (inst ++ [NoOp (Var val)], jt);}
preprocessor (Cons (SexprInt val) rest) = do {(inst,jt) <- get; put (inst ++ [NoOp (AGLfloat (fromIntegral val))],jt); preprocessor rest;}
preprocessor (Cons (SexprFloat val) rest) = do {(inst,jt) <- get; put (inst ++ [NoOp (AGLfloat val)],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "penup") Nil) rest) = do {(inst,jt) <- get; put (inst ++ [Penup],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "pendown") Nil) rest) = do {(inst,jt) <- get; put (inst ++ [Pendown],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "stop") Nil) rest) = do {(inst,jt) <- get; put (inst ++ [Stop],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "push") Nil) rest) = do {(inst,jt) <- get; put (inst ++ [Push],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "pop") Nil) rest) = do {(inst,jt) <- get; put (inst ++ [Pop],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "forward") (Cons (SexprInt i) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [Forward (AGLfloat (fromIntegral i))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "forward") (Cons (Symbol var) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [Forward (Var var)],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "forward") arithSexpr) rest) = do 
    (inst,jt) <- get
    put ([],jt)
    preprocessor arithSexpr;
    (arithInst,_) <- get  
    put(inst ++ [Forward $ getexpr $ head arithInst], jt)
    preprocessor rest
preprocessor (Cons (Cons (Symbol "backward") (Cons (SexprInt i) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [Backward (AGLfloat (fromIntegral i))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "backward") (Cons (Symbol var) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [Backward (Var var)],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "backward") arithSexpr) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor arithSexpr
  (arithInst,_) <- get
  put(inst ++ [Backward $ getexpr $ head arithInst],jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "right") (Cons (SexprInt i) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [MyRight (AGLfloat (fromIntegral i))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "right") (Cons (Symbol var) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [MyRight (Var var)],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "right") arithSexpr) rest) = do 
  (inst,jt) <- get
  put([],jt)
  preprocessor arithSexpr
  (arithInst,_) <- get
  put(inst ++ [MyRight $ getexpr $ head arithInst],jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "left") (Cons (SexprInt i) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [MyLeft (AGLfloat (fromIntegral i))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "left") (Cons (Symbol var) Nil)) rest) = do{(inst,jt) <- get; put (inst ++ [MyLeft (Var var)],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "left") arithSexpr) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor arithSexpr
  (arithInst,_) <- get
  put(inst ++ [MyLeft $ getexpr $ head arithInst],jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "color") (Cons (Symbol var) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [MyColor (Var var)],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "color") (Cons (SexprInt i) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [MyColor (AGLfloat (fromIntegral i))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "color") (Cons (SexprFloat i) Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [MyColor (AGLfloat i)],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "color") arithSexpr) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor arithSexpr
  (arithInst,_) <- get
  put(inst ++ [MyColor $ getexpr $ head arithInst],jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "repeat") (Cons (SexprInt i) sexpr)) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor sexpr
  (repeatInst,_) <- get
  put(inst ++ [MyRepeat (AInt i) repeatInst],jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "repeat") (Cons (Symbol var) sexpr)) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor sexpr
  (repeatInst,_) <- get
  put(inst ++ [MyRepeat (Var var) repeatInst], jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "setxy") arithSexpr) rest) = do
  (inst,jt) <- get
  let first = car arithSexpr
  let second = car $ cdr arithSexpr
  let temp1 = if elem ')' (show first) then "a" else "i"
  let temp2 = if elem ')' (show second) then "a" else "i"
  let type1 = if elem '.' (show first) && temp1 == "i" then "f" else temp1
  let type2 = if elem '.' (show second) && temp2 == "i" then "f" else temp2
  dataOne <- (setXYHelper first type1)
  dataTwo <- (setXYHelper second type2)
  put(inst ++ [SetXY dataOne dataTwo], jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "make") (Cons (Symbol var) (Cons (SexprInt i) Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [Make var (AGLfloat (fromIntegral i))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "make") (Cons (Symbol var) (Cons (SexprFloat i) Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [Make var (AGLfloat i)],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "make") (Cons (Symbol var) arithSexpr)) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor arithSexpr
  (arithInst,_) <- get
  put(inst ++ [Make var $ getexpr $ head arithInst],jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "if") sexpr) rest) = do
  (inst,jt) <- get
  let a = car sexpr
  let b = car $ cdr sexpr
  let c = cdr $ cdr sexpr
  put([],jt)
  preprocessor (Cons b Nil)
  (test,_) <- get
  put([],jt)
  preprocessor (Cons a Nil)
  (condInstTmp,_) <- get
  let condInst = getcond $ head condInstTmp
  ifInst <- ifHelper b
  ifelseInst<- elseHelper c
  let instnew = if (isNil c) then [If condInst ifInst] else [IfElse condInst ifInst ifelseInst]
  put(inst ++ instnew,jt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "to") (Cons (Symbol funcName) (Cons (Cons (Symbol arg1) Nil) sexpr))) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor sexpr
  (subroute,_) <- get
  let params = [NoOp (Var arg1)]
  let updatedJt = jt ++ [(funcName, (params, subroute))]
  put(inst,updatedJt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "to") (Cons (Symbol funcName) (Cons (Cons (Symbol arg1) (Cons (Symbol arg2) Nil)) sexpr))) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor sexpr
  (subroute,_) <- get
  let params = [NoOp (Var arg1), NoOp (Var arg2)]
  let updatedJt = jt ++ [(funcName, (params, subroute))]
  put(inst,updatedJt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "to") (Cons (Symbol funcName) (Cons (Cons (Symbol arg1) (Cons (Symbol arg2) (Cons (Symbol arg3) Nil))) sexpr))) rest) = do 
  (inst,jt) <- get
  put([],jt)
  preprocessor sexpr
  (subroute,_) <- get
  let params = [NoOp (Var arg1), NoOp (Var arg2), NoOp (Var arg3)]
  let updatedJt = jt ++ [(funcName, (params, subroute))]
  put(inst,updatedJt)
  preprocessor rest
preprocessor (Cons (Cons (Symbol "to") (Cons (Symbol funcName) (Cons (Cons (Symbol arg1) (Cons (Symbol arg2) (Cons (Symbol arg3) (Cons (Symbol arg4) Nil)))) sexpr))) rest) = do
  (inst,jt) <- get
  put([],jt)
  preprocessor sexpr
  (subroute,_) <- get
  let params = [NoOp (Var arg1), NoOp (Var arg2), NoOp (Var arg3), NoOp (Var arg4)]
  let updatedJt = jt ++ [(funcName, (params, subroute))]
  put(inst,updatedJt)
  preprocessor rest

-- --arithmetic
preprocessor (Cons (Cons (Symbol "+") (Cons sexpr1 (Cons sexpr2 Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [ArithWrapper $ Arithmetic (Cons (Symbol "+") (Cons sexpr1 (Cons sexpr2 Nil)))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "-") (Cons sexpr1 (Cons sexpr2 Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [ArithWrapper $ Arithmetic (Cons (Symbol "-") (Cons sexpr1 (Cons sexpr2 Nil)))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "*") (Cons sexpr1 (Cons sexpr2 Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [ArithWrapper $ Arithmetic (Cons (Symbol "*") (Cons sexpr1 (Cons sexpr2 Nil)))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "/") (Cons sexpr1 (Cons sexpr2 Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [ArithWrapper $ Arithmetic (Cons (Symbol "/") (Cons sexpr1 (Cons sexpr2 Nil)))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "-") (Cons sexpr Nil)) rest) = do {(inst,jt) <- get; put (inst ++ [ArithWrapper $ Arithmetic (Cons (Symbol "-") (Cons (SexprFloat 0.0) (Cons sexpr Nil)))],jt); preprocessor rest;}

-- --conditionals
preprocessor (Cons (Cons (Symbol "<") (Cons sexpr1 (Cons sexpr2 Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [CondWrapper $ Conditional (Cons (Symbol "<") (Cons sexpr1 (Cons sexpr2 Nil)))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol ">") (Cons sexpr1 (Cons sexpr2 Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [CondWrapper $ Conditional (Cons (Symbol ">") (Cons sexpr1 (Cons sexpr2 Nil)))],jt); preprocessor rest;}
preprocessor (Cons (Cons (Symbol "=") (Cons sexpr1 (Cons sexpr2 Nil))) rest) = do {(inst,jt) <- get; put (inst ++ [CondWrapper $ Conditional (Cons (Symbol "=") (Cons sexpr1 (Cons sexpr2 Nil)))],jt); preprocessor rest;}

-- -- Any non-matched symbols should be function calls. 
preprocessor (Cons (Cons (Symbol fCall) sexpr) rest) = do
 
 (inst,jt) <- get
 put([],jt)
 preprocessor sexpr
 (args,_) <- get
 put(inst ++ [Call fCall args],jt)
 preprocessor rest

-- -- Params to functions. 
preprocessor (Cons (Symbol val) rest) = do {(inst,jt) <- get; put (inst ++ [NoOp (Var val)], jt); preprocessor rest;}

-- -- Triple nested cons are reduced by one level.
preprocessor (Cons rest nil) = do {(inst,jt) <- get; put (inst,jt); preprocessor rest;}


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

repeatHelper :: [Instruction] -> Int -> GSM (GraphicsState)
repeatHelper inst 0 = do {(inst,ps,stack,g,jt,env,vs) <- get; return ((inst,ps,stack,g,jt,env,vs));}
repeatHelper inst n = do
  graphicsTranslator inst
  (_,ps,stack,g,jt,env,vs) <- get
  put(inst,ps,stack,g,jt,env,vs)
  repeatHelper inst (n - 1)

recurHelper :: [Instruction] -> GSM (GraphicsState)
recurHelper inst = do
  graphicsTranslator inst
  (instcpy,ps,stack,g,jt,env,vs) <- get
  return (instcpy,ps,stack,g,jt,env,vs)

graphicsTranslator :: [Instruction] -> GSM ()
graphicsTranslator [] = return ()
graphicsTranslator (Stop:rest) = do {(instcpy,(s,c,p),stack,g,jt,env,vs) <- get; put (instcpy,(s,c,p),stack,g,jt,env,"off")}
graphicsTranslator (Push:rest) = do {(instcpy,(s,c,p),stack,g,jt,env,vs) <- get; put (instcpy,(s,c,p),(s,c,p):stack,g,jt,env,vs); graphicsTranslator rest}
graphicsTranslator (Pop:rest) = do
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let oldpstate@(sold,cold,pold@(xold,yold,oldang)) = (s,c,p)
  let newpstate@(snew,cnew,pnew@(xnew,ynew,newang)) = head stack
  let newstack = tail stack
  let ang = getAngle pold (xnew,ynew)
  let dist = getDist pold (xnew,ynew)
  let gtmp = (if xnew == xold && ynew == xold then [] else [(Paint c $ Bend $ ang), Paint c $ Invisible dist, (Paint c $ Bend $ -ang)]) ++ [(Paint c $ Bend $ (newang - oldang))]
  let gnew = g ++ gtmp
  put(instcpy,newpstate,newstack,gnew,jt,env,vs)
  graphicsTranslator rest


graphicsTranslator ((MyColor (Var var)):rest) = do
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = getval $ resolveVar env var
  let cnew = hueToRGB val
  if isOn vs then do {put (instcpy,(s,cnew,p),stack,g,jt,env,vs); graphicsTranslator rest} 
    else return ()        

graphicsTranslator ((MyColor (Arithmetic arithSexpr)):rest) = do
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
  let cnew = hueToRGB val
  if isOn vs then do {put (instcpy,(s,cnew,p),stack,g,jt,env,vs); graphicsTranslator rest}
    else return ()

graphicsTranslator ((MyColor val):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let cnew = hueToRGB (getval val)
  if isOn vs then do {put (instcpy,(s,cnew,p),stack,g,jt,env,vs); graphicsTranslator rest}
    else return () 

graphicsTranslator (Penup:rest) = do {(instcpy,(s,c,p),stack,g,jt,env,vs) <- get; put (instcpy,("up",c,p),stack,g,jt,env,vs); graphicsTranslator rest}

graphicsTranslator (Pendown:rest) = do {(instcpy,(s,c,p),stack,g,jt,env,vs) <- get; put (instcpy,("down",c,p),stack,g,jt,env,vs); graphicsTranslator rest}

graphicsTranslator ((Make var (Arithmetic arithSexpr)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = (AGLfloat $ arithmeticSolver arithSexpr env)
  let updatedEnv = updateEnv [(var,val)] env
  if isOn vs then do {put (instcpy,(s,c,p),stack,g,jt,updatedEnv,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((Make var val):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let updatedEnv = updateEnv [(var,val)] env
  if isOn vs then do {put (instcpy,(s,c,p),stack,g,jt,updatedEnv,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((Forward (Var var)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = getval $ resolveVar env var
  let pnew = (updatePoint p "F" val)
  let gnew = g ++ [Paint c $ (if s == "down" then Straight val else Invisible val)]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()        

graphicsTranslator ((Forward (Arithmetic arithSexpr)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get 
  let val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
  let pnew = (updatePoint p "F" val)
  let gnew = g ++ [Paint c $ (if s == "down" then Straight val else Invisible val)]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((Forward val):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get 
  let pnew = (updatePoint p "F" ((getval val) :: Float))
  let gnew = g ++ [Paint c $ (if s == "down" then Straight (getval val) else Invisible (getval val))]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()
 
graphicsTranslator ((Backward (Var var)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = getval $ resolveVar env var
  let pnew = (updatePoint p "B" val)
  let gnew = g ++ [Paint c $ (if s == "down" then Straight (-val) else Invisible (-val))]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((Backward (Arithmetic arithSexpr)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
  let pnew = (updatePoint p "B" val)
  let gnew = g ++ [Paint c $ (if s == "down" then Straight (-val) else Invisible (-val))]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()  

graphicsTranslator ((Backward val):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let pnew = (updatePoint p "B" ((getval val) :: Float))
  let gnew = g ++ [Paint c $ (if s == "down" then Straight (-(getval val)) else Invisible (-(getval val)))]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()
 

graphicsTranslator ((MyRight (Var var)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = getval $ resolveVar env var
  let pnew = (updatePoint p "R" val)
  let gnew = g ++ [Paint c $ Bend (-val)]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((MyRight (Arithmetic arithSexpr)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get 
  let val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
  let pnew = (updatePoint p "R" val)
  let gnew = g ++ [Paint c $ Bend (-val)]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((MyRight val):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let pnew = (updatePoint p "R" ((getval val) :: Float))
  let gnew = g ++ [Paint c $ Bend (-(getval val))]
  if isOn vs then do{put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest}  
    else return ()

graphicsTranslator ((MyLeft (Var var)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = getval $ resolveVar env var
  let pnew = (updatePoint p "L" val)
  let gnew = g ++ [Paint c $ Bend val]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((MyLeft (Arithmetic arithSexpr)):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let val = getval $ (AGLfloat $ arithmeticSolver arithSexpr env)
  let pnew = (updatePoint p "L" val)
  let gnew = g ++ [Paint c $ Bend val]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((MyLeft val):rest) = do 
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let pnew = (updatePoint p "L" ((getval val) :: Float))
  let gnew = g ++ [Paint c $ Bend (getval val)]
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((MyRepeat (Var var) inst):rest) = do
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let i = floor $ getval $ resolveVar env var
  if isOn vs then do {(instcpy,(s,c,p),stack,g,jt,env,vs) <- repeatHelper inst i; graphicsTranslator rest;}
    else return ()

graphicsTranslator ((MyRepeat (AInt i) inst):rest) = do
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  if isOn vs then do {(instcpy,(s,c,p),stack,g,jt,env,vs) <- repeatHelper inst i; graphicsTranslator rest;} 
    else return ()

graphicsTranslator ((SetXY a b):rest) = do 
  (instcpy,(s,c,p@(x,y,oldang)),stack,g,jt,env,vs) <- get 
  let xnew = getval $ (if isArith a then AGLfloat $ (arithmeticSolver (getsexpr a) env) else a)
  let ynew = getval $ (if isArith b then AGLfloat $ (arithmeticSolver (getsexpr b) env) else b)
  let fltpnt = (xnew, ynew)
  let ang = getAngle p fltpnt
  let dist = getDist p fltpnt
  let gnew = g ++ (if xnew == x && ynew == y then [] else [(Paint c $ Bend $ ang), Paint c $ (if s == "down" then Straight dist else Invisible dist), (Paint c $ Bend $ -ang)])
  let pnew = (xnew, ynew, oldang)
  if isOn vs then do {put (instcpy,(s,c,pnew),stack,gnew,jt,env,vs); graphicsTranslator rest} 
    else return ()

graphicsTranslator ((If (Conditional condsexpr) inst):rest) = do
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let condResult = conditionalResolver condsexpr env
  if (isOn vs) && condResult then do
    put([],(s,c,p),stack,[],jt,env,vs)
    (_,(snew,cnew,pnew),stacknew,gacc,_,_,vsnew) <- recurHelper inst
    let gnew = g ++ gacc
    put(instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vsnew)
    graphicsTranslator rest
  else if (isOn vs) && (not condResult) then do
    put(instcpy,(s,c,p),stack,g,jt,env,vs)
    graphicsTranslator rest
  else return ()

graphicsTranslator ((IfElse (Conditional condsexpr) ifinst elseinst):rest) = do
  (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
  let condResult = conditionalResolver condsexpr env
  if (isOn vs) && condResult then do
    put([],(s,c,p),stack,[],jt,env,vs)
    (_,(snew,cnew,pnew),stacknew,gacc,_,_,vsnew) <- recurHelper ifinst
    let gnew = g ++ gacc
    put(instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vsnew)
    graphicsTranslator rest
  else if (isOn vs) && (not condResult) then do
    put([],(s,c,p),stack,[],jt,env,vs)
    (_,(snew,cnew,pnew),stacknew,gacc,_,_,vsnew) <- recurHelper elseinst
    let gnew = g ++ gacc
    put(instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vsnew)
    graphicsTranslator rest
  else return ()

graphicsTranslator ((Call funcName args):rest) = do
    (instcpy,(s,c,p),stack,g,jt,env,vs) <- get
    if isOn vs then do
      let subProc = getSubRoute funcName jt
      let params = getParams subProc
      let subProcInst = getSubRouteInst subProc
      let resolvedArgs = resolveArithArgs (resolveArgs args env) env
      let bindings = zip params resolvedArgs
      let subProcEnv = updateEnv bindings []
      put([],(s,c,p),stack,[],jt,subProcEnv,vs)
      (_,(snew,cnew,pnew),stacknew,gacc,_,_,vsnew) <- recurHelper subProcInst
      let gnew = g ++ gacc
      put(instcpy,(snew,cnew,pnew),stacknew,gnew,jt,env,vsnew)
      graphicsTranslator rest
    else return ()

getStart :: String -> String
getStart "broccoli" = "(define broccoli'((to broccoli (x y)(penup)(left 90)(forward 50)(right 90)(pendown)(broccoli1 x y))(to broccoli1 (x y) (if (< x y) (stop) ((square x) (forward x) (left 45) (broccoli1 (/ x (sqrt 2)) y) (penup) (backward (/ x (sqrt 2))) (left 45) (pendown)(backward x))))(to square (x) (repeat 4 (forward x) (right 90)))(broccoli 100 1)))"
getStart "fancy-spiral" = "(define fancy-spiral'((to fancy-spiral (size angle)(if (> size 200)(stop))(color (* size (/ 360 200)))(forward size)(right angle)(fancy-spiral (+ size 1) angle))(penup)(forward 120)(pendown)(fancy-spiral 0 91)))"
getStart "lissajous" = "(define lissajous '((to lissajous (a b c t)(penup)(setxy (* (cos c) 75) 100)(pendown)(repeat 364 (color t)(setxy (* (cos (+ (* t a) c)) 75) (+ (* (sin (* t b)) 75) 100))(make t (+ t 1))))(lissajous 0.1396 -0.12215 0.2094 0)))"
getStart "hexfield" = "(define hexfield '((to hexfield (n c)(if (= n 1)(repeat 6 (forward 20) (left 60)(color (* c 60))(make c (+ c 1))) (repeat 6 (forward 20) (push) (right 180) (hexfield (- n 1) 0)(pop)(left 60))))(penup)(forward 100)(pendown)(right 90)(hexfield 3 0)))"
getStart "circles" = "(define circles'((to circle (seg clr)(if (< seg 1)(forward 0)(repeat 5(repeat 8(make clr (+ clr 10))(forward seg)(right 9))(right 180)(circle (/ seg 2) (+ clr 47))(right 180))))(penup)(setxy -50 200)(pendown)(circle 10 0)))"
getStart "stars" = "(define stars'((to stars (side angle max) (repeat 5 (star side angle max 1))) (to star (side angle max count) (repeat max (forward (* side count)) (right angle) (make count (+ count 1))))(penup)(forward 50)(pendown)(stars 15 144 8)(penup)(backward 50)))"
getStart "starfish" = "(define starfish '((to starfish (side angle inc) (repeat 90 (forward side) (right angle) (make angle (+ angle inc)))) (penup) (forward 50) (pendown) (starfish 30 2 20)))))"
getStart "hilbert" = "(define hilbert'((to hilbert (size level parity)(if (> level 0)((left (* parity 90))(hilbert size (- level 1) (- parity))(forward size)(right (* parity 90))(hilbert size (- level 1) parity)(forward size)(hilbert size (- level 1) parity)(right (* parity 90))(forward size)(hilbert size (- level 1) (- parity))(left (* parity 90)))))(hilbert 10 4 1)))"
getStart "koch" = "(define koch'((to koch (n) (if (= n 1) (forward 8) (forward 3) ((koch (- n 1)) (left 60) (koch (- n 1)) (right 120) (koch (- n 1)) (left 60) (koch (- n 1)))))(repeat 3 (koch 4)(right 120))))"
getStart "tree" = "(define tree'((to tree (depth count)(forward (* depth 20))(right 90)(if (> depth 1)(repeat 5 (push)(left (* count 30))(color (* 60 count)) (tree (- depth 1) 1)(pop)(make count (+ count 1)))))(tree 4 1)))"
getStart "e1" = "(define foo '((forward 50)))"
getStart "e2" = "(define foo '(((forward 50)(right 90)(forward 50))))"
getStart "e3" = "(define foo '(((forward 50)(left 90)(forward 50))))"
getStart "e4" = "(define foo '(((forward 50)(right 90)(backward 50))))"
getStart "e5" = "(define foo '((repeat 5 (penup)(forward 5)(pendown)(forward 5))))"
getStart "e6" = "(define foo '((repeat 5 (forward 50)(right (/ 360 5)))))"
getStart "e7" = "(define foo '(((to square (side)(repeat 4 (forward side) (right 90)))(square 50))))"
getStart "e8" = "(define foo '(((to spiral(side angle max count)(repeat max (forward (* side count))(right angle) (make count (+ count 1))))(penup)(forward 70)(pendown)(spiral 0.05 10 180 0))))"
getStart "e9" = "(define foo '(((right 30)(color 60) (forward 100)(right 120)(color 300)(forward 100)(right 120)(color 180)(forward 80))))"
getStart "e10" = "(define foo '(((to circle (h r) (repeat 90 (color h) (make r (* (/ h 360) (* 2 3.1416)))(setxy (* (cos r) 50) (+ (* (sin r) 50) 50)) (make h (+ h 4)))) (penup)(setxy 50 50) (pendown) (circle 0 0))))"
getStart "e11" = "(define foo '(((color 200) (forward 25) (push) (color 0) (right 45) (forward 50) (pop) (forward 25))))"
getStart  _ = "invalid"

main = do
  (progname, args) <- getArgsAndInitialize
  if (length args) == 0 then do
    putStrLn $ "You must provide the name of the example you wish to run. \n Example: ./basic_interp broccoli"
    return ()
  else if getStart (head args) == "invalid" then do
      putStrLn $ "Invalid example name."
      return ()
    else do
      putStrLn $ "Running example: " ++ (show (head args))
      let startString = getStart (head args) 
      createWindow "Haskell Plumbing Graphics"
      (_,(instructionStream,jt)) <- runStateT (preprocessor (stripHeader $ p startString)) ([],[])
      (_,(a,b,c,d,e,f,g)) <- runStateT (graphicsTranslator instructionStream) ([], ("down",green,(0.0,0.0,90.0)),[("down",green,(0.0,0.0,90.0))],[],jt,[],"on")
      let graphicsInstructionStream = [Bend 90] ++ d  
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
