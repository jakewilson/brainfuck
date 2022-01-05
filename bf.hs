module Bf where

type Tape = [Int]

data BFIns = BFOutput
           | BFInput
           | BFIncP
           | BFDecP
           | BFIncB
           | BFDecB
           | BFJumpF
           | BFJumpB
           deriving Show

data BFState = BFState
  { tape      :: Tape
  , pointer   :: Int
  , cmdState  :: CmdState
  }

instance Show BFState where
  show (BFState tape _ _) = show $ take 20 tape

data CmdState = CmdState
  { prog     :: String
  , progLen  :: Int
  , ip       :: Int
  }
  deriving Show

size :: Int
size = 30000

modify :: [Int] -> Int -> (Int -> Int) -> [Int]
modify []     _ _   = error "out of bounds array access"
modify (t:ts) 0 f   = f t : ts
modify (t:ts) idx f = t : modify ts (idx - 1) f

-- increments the ip until a ] is hit
goToC :: Char -> BFState -> BFState
goToC c b = b { cmdState = goToC' c (cmdState b) }
  where
    goToC' :: Char -> CmdState -> CmdState
    goToC' c (CmdState _ len ip)
      | ip >= len || ip < 0 = error $ "improperly placed bracket at pos " ++ show ip
    goToC' c s@(CmdState prog _ ip) = if cmd == c then move 1 s else goToC' c $ move dir s
      where
        cmd = prog !! ip
        dir = if c == '[' then -1 else 1

-- replaces the CmdState in BFState with the provided CmdState
updateCmd :: BFState -> CmdState -> BFState
updateCmd (BFState t p _) = BFState t p

exec :: Char -> BFState -> BFState
exec '-' (BFState tape pointer c) = BFState (modify tape (pointer `mod` size) (subtract 1)) pointer (move 1 c)
exec '+' (BFState tape pointer c) = BFState (modify tape (pointer `mod` size) (+ 1)) pointer (move 1 c)
exec '>' (BFState tape pointer c) = BFState tape (pointer + 1) (move 1 c)
exec '<' (BFState tape pointer c) = BFState tape (pointer - 1) (move 1 c)

exec '[' b = if skip then goToC ']' b else updateCmd b (move 1 $ cmdState b)
  where
    skip = get b == 0

exec ']' b = goToC '[' b
exec _ bf = bf

initialBFState :: String -> BFState
initialBFState prog = BFState (replicate size 0) 0 (initialCmdState prog)

initialCmdState :: String -> CmdState
initialCmdState prog = CmdState prog (length prog) 0

get :: BFState -> Int
get (BFState tape pointer _) = tape !! (pointer `mod` size)

move :: Int -> CmdState -> CmdState
move dir (CmdState p l ip) = CmdState p l (ip + dir)

main :: IO ()
main = do
  prog <- readFile "prog.bf"
  mainLoop $ initialBFState prog

mainLoop :: BFState -> IO ()
mainLoop (BFState _ _ (CmdState _ len ip))
  | ip >= len || ip < 0 = return ()

mainLoop b@(BFState tape pointer c@(CmdState prog _ ip)) = do
  let cmd = prog !! ip
  print cmd
  let s = exec cmd b
  getChar
  print s
  mainLoop s
