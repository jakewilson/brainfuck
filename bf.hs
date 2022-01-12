import Tape

import Data.Char

type Program = Tape Char
type Memory  = Tape Int

data State = State
  { program :: Program
  , memory  :: Memory
  }
  deriving Show

data Result =
    Output Char State
  | Input (Char -> State)
  | Continue State
  | Stop

data LoopType = Start | End

goto :: LoopType -> Program -> Program
goto Start = start 0
  where
    start :: Int -> Program -> Program
    start lc prog
      | cursor prog == '[' && lc == 0 = prog
      | cursor prog == '['            = start (lc - 1) $ left prog
      | cursor prog == ']'            = start (lc + 1) $ left prog
      | otherwise                     = start lc $ left prog

goto End = end 0
  where
    end :: Int -> Program -> Program
    -- goto' takes a loop counter arg for proper handling
    -- of nested loops so [[]] will correctly go to the second
    -- end bracket instead of stopping immediately at the first
    end lc prog
      | cursor prog == ']' && lc == 0 = prog
      | cursor prog == ']'            = end (lc - 1) $ right prog
      | cursor prog == '['            = end (lc + 1) $ right prog
      | otherwise                     = end lc $ right prog

loopStart :: Program -> Memory -> Program
loopStart prog mem = if skip then goto End (right prog) else prog
  where skip = cursor mem == 0

loopEnd :: Program -> Program
loopEnd prog = goto Start (left prog)

run :: State -> Result
run (State prog _) | end prog = Stop
run (State prog mem) = case cursor prog of
  '+' -> Continue $ State (right prog) (update (+ 1) mem)
  '-' -> Continue $ State (right prog) (update (subtract 1) mem)
  '>' -> Continue $ State (right prog) (right mem)
  '<' -> Continue $ State (right prog) (left mem)
  '[' -> Continue $ State (right $ loopStart prog mem) mem
  ']' -> Continue $ State (loopEnd prog) mem
  '.' -> Output (chr $ cursor mem) $ State (right prog) mem
  ',' -> Input (\c -> State (right prog) (modify (ord c) mem))
  _   -> Continue $ State (right prog) mem -- any other char is a comment

size :: Int
size = 100

main :: IO ()
main = do
  input <- readFile "hello_world.bf"
  let memory = tape $ replicate size 0
  let prog   = tape input
  mainLoop $ State prog memory

mainLoop :: State -> IO ()
mainLoop state = case run state of
  Continue state' -> mainLoop state'
  Input put       -> getChar >>= mainLoop . put
  Output c state' -> putChar c >> mainLoop state'
  Stop            -> return ()
