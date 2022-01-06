module Tape where

data Tape a = Tape [a] [a]

instance Show a => Show (Tape a) where
  show (Tape xs (y:ys)) = show (reverse xs) ++ " " ++ show y ++ " " ++ show ys
  show (Tape xs []) = show $ reverse xs

tape :: [a] -> Tape a
tape [] = Tape [] []
tape xs = Tape [] xs

right :: Tape a -> Tape a
right (Tape xs (y:ys)) = Tape (y:xs) ys
right t                = t

left :: Tape a -> Tape a
left (Tape (x:xs) ys) = Tape xs (x:ys)
left t                = t

end :: Tape a -> Bool
end (Tape xs []) = True
end _            = False

begin :: Tape a -> Bool
begin (Tape [] ys) = True
begin _            = False

cursor :: Tape a -> a
cursor (Tape (x:xs) []) = x
cursor (Tape _ (y:ys))  = y
cursor t                = error "empty tape"

update :: (a -> a) -> Tape a -> Tape a
update f (Tape xs (y:ys)) = Tape xs (f y:ys)
update f (Tape xs [])     = error "updating past the end of the tape"

modify :: a -> Tape a -> Tape a
modify v = update (const v)
