module Main where

import Data.Bifunctor
import Data.List

type Shape = [(Int, Int)]

ps :: Shape
ps = [(i, j) | i <- [1 .. 6], j <- [1 .. 4]] \\ [(1, 4), (5, 2)]

half :: Int -> Shape -> [(Shape, Shape)]
half k = go k k
 where
  go 0 _ xs = [([], xs)]
  go _ 0 xs = [(xs, [])]
  go m n (x : xs) =
    [(x : ys, zs) | (ys, zs) <- go (m - 1) n xs]
      ++ [(ys, x : zs) | (ys, zs) <- go m (n - 1) xs]

trans, rev, move :: Shape -> Shape
trans = sort . map (\(x, y) -> (y, x))
rev = sort . map (second (5 -))
move = uncurry zip . bimap f f . unzip
 where
  f xs = map (subtract (minimum xs - 1)) xs

cong :: Shape -> [Shape]
cong a = map move [a, b, c, d, e, f, g, h]
 where
  b = trans a
  c = rev a
  d = rev b
  e = trans c
  f = trans d
  g = rev e
  h = rev f

eq :: (Shape, Shape) -> Bool
eq (a, b) = or [x == y | x <- cong a, y <- cong b]

ans :: [(Shape, Shape)]
ans = filter eq $ half 11 ps

main :: IO ()
main = do
  mapM_ print ans
