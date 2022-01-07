import Tape

import Data.Char
import Debug.Trace

type Program = Tape Char
type Memory  = Tape Int

data Action =
    Output Char
  | Input (Char -> Memory -> Memory)
  | Continue
  | Stop

data LoopType = Start | End

goto :: LoopType -> Program -> Program
goto = goto' 0
  where
    goto' :: Int -> LoopType -> Program -> Program
    -- goto' takes a loop counter arg for proper handling
    -- of nested loops so [[]] will correctly go to the second
    -- end bracket instead of stopping immediately at the first
    goto' lc Start prog
      | cursor prog == '[' && lc == 0 = prog
      | cursor prog == '['            = goto' (lc - 1) Start $ left prog
      | cursor prog == ']'            = goto' (lc + 1) Start $ left prog
      | otherwise                     = goto' lc Start $ left prog
    goto' lc End prog
      | cursor prog == ']' && lc == 0 = prog
      | cursor prog == ']'            = goto' (lc - 1) End $ right prog
      | cursor prog == '['            = goto' (lc + 1) End $ right prog
      | otherwise                     = goto' lc End $ right prog

loopStart :: Program -> Memory -> Program
loopStart prog mem = if skip then goto End (right prog) else prog
  where skip = cursor mem == 0

loopEnd :: Program -> Program
loopEnd prog = goto Start (left prog)

run :: Program -> Memory -> (Action, Program, Memory)
run prog mem | end prog = (Stop, prog, mem)
run prog mem =
  case cursor prog of
    '+' -> (Continue, right prog, update (+ 1) mem)
    '-' -> (Continue, right prog, update (subtract 1) mem)
    '>' -> (Continue, right prog, right mem)
    '<' -> (Continue, right prog, left mem)
    '[' -> (Continue, right $ loopStart prog mem, mem)
    ']' -> (Continue, loopEnd prog, mem)
    '.' -> (Output $ chr (cursor mem), right prog, mem)
    ',' -> (Input (modify . ord), right prog, mem)
    _   -> error "wat"

size :: Int
size = 100

removeComments :: String -> String
removeComments = filter (`elem` "<>+-[],.")

progTape :: String -> Program
progTape = tape . removeComments

main :: IO ()
main = do
  input <- readFile "hello_world.bf"
  let memory = tape $ replicate size 0
  let prog   = progTape input
  mainLoop prog memory

mainLoop :: Program -> Memory -> IO ()
mainLoop prog mem = do
  let (action, prog', mem') = run prog mem
  case action of
    Continue      -> mainLoop prog' mem'
    Stop          -> return ()
    (Input f)  -> do
      c <- getChar
      let mem'' = f c mem'
      mainLoop prog' mem''
    (Output c) -> do
      putChar c
      mainLoop prog' mem'
