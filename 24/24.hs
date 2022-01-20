import           Control.Monad.Trans.State.Lazy
import qualified Data.Char                      as C
import qualified Data.List                      as L

data Var = W | X | Y | Z deriving (Show)
data Operand = I Int | V Var deriving (Show)
data Op = Inp { _a::Var }
        | Add { _a::Var, _b::Operand }
        | Mul { _a::Var, _b::Operand }
        | Div { _a::Var, _b::Operand }
        | Mod { _a::Var, _b::Operand }
        | Eql { _a::Var, _b::Operand }
          deriving (Show)

data Register = Register
    { _inps :: [Int]
    , _w    :: Int
    , _x    :: Int
    , _y    :: Int
    , _z    :: Int
    }

instance Show Register where
    show r = "w: " ++ w ++ ", x: " ++ x ++ ", y: " ++ y ++ ", z: " ++ z
      where w = show (_w r)
            x = show (_x r)
            y = show (_y r)
            z = show (_z r)

evalOperand :: Register -> Operand -> Int
evalOperand _ (I i) = i
evalOperand r (V v) = getAt r v

evalOp :: Register -> Op -> Register
evalOp r (Inp v) = putAt r' v x
  where
    popInp :: Register -> (Register, Int)
    popInp r = (r { _inps=tail $ _inps r }, head $ _inps r)

    (r', x) = popInp r
evalOp r op = putAt r (_a op) x'
  where
    e = evalOperand r
    f = case op of
            (Add a b) -> (+)
            (Div a b) -> div
            (Mul a b) -> (*)
            (Mod a b) -> rem
            (Eql a b) -> \x y -> if x == y then 1 else 0
            _         -> error "uhoh"
    x = getAt r (_a op)
    x' = f x (e (_b op))

getAt :: Register -> Var -> Int
getAt r W = _w r
getAt r X = _x r
getAt r Y = _y r
getAt r Z = _z r

putAt :: Register -> Var -> Int -> Register
putAt r W a = r { _w=a }
putAt r X a = r { _x=a }
putAt r Y a = r { _y=a }
putAt r Z a = r { _z=a }

makeNewRegister :: [Int] -> Register
makeNewRegister inps = Register
    { _inps=inps
    , _w=0
    , _x=0
    , _y=0
    , _z=0
    }

evaluateOps :: [Op] -> State Register Register
evaluateOps ops = do
    r <- get
    put $ L.foldl' evalOp r ops
    get

main :: IO ()
main = do
    ops <- map parseOp . lines <$> readFile "input.txt"
    testInputs ops 99999999999999

-- TODO probably better to use `Text.Read` or something.
intify :: String -> Int
intify s = read s :: Int

parseOp :: String -> Op
parseOp s
    | "inp" `L.isPrefixOf` s = Inp a
    | "add" `L.isPrefixOf` s = Add a b
    | "mul" `L.isPrefixOf` s = Mul a b
    | "div" `L.isPrefixOf` s = Div a b
    | "mod" `L.isPrefixOf` s = Mod a b
    | "eql" `L.isPrefixOf` s = Eql a b
    | otherwise = error "uhoh"
  where
    parts = tail . words $ s
    a = parseVar $ head parts
    b = parseOperand . head . tail $ parts

parseVar :: String -> Var
parseVar "w" = W
parseVar "x" = X
parseVar "y" = Y
parseVar "z" = Z
parseVar _   = error "uhoh"

parseOperand :: String -> Operand
parseOperand s
    | any C.isDigit s = I (read s :: Int)
    | otherwise = V (parseVar s)

numToDigitList :: Int -> [Int]
numToDigitList = map C.digitToInt . show

testInput :: [Op] -> [Int] -> (Bool, Register)
testInput ops input =
    let register = makeNewRegister input
        (_, r) = runState (evaluateOps ops) register
    in (_z r == 0, r)

testInputs :: [Op] -> Int -> IO ()
testInputs ops input
    | input < 10000000000000 = error "done?"
    | otherwise = do
        let (finished, r) = testInput ops (numToDigitList input)
        putStrLn $ show input ++ " / " ++ show r
        if finished
           then return ()
           else testInputs ops (input - 1)
