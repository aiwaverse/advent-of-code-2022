module DayNine where


data Motion = L Int | R Int | U Int | D Int deriving (Show, Read)

move :: ((Int, Int), (Int, Int)) -> Motion -> ((Int, Int), (Int, Int))
move (headPos, (tailX, tailY)) mt = (newHeadPost, undefined)
  where newHeadPost = moveHead headPos mt

moveHead :: (Int, Int) -> Motion -> (Int, Int)
moveHead (headX, headY) mt = case mt of
  R i -> (step headX i, headY)
  L i -> (step headX (-i), headY)
  U i -> (headX, step headY i)
  D i -> (headX, step headY (-i))

step :: Int -> Int -> Int
step x y = if x + y < 0 then 0 else x + y