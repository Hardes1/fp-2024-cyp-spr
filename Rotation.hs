module Rotation(rotate) where


rotate :: Int -> [a] -> [a]
rotate val arr
  | null arr = arr 
  | otherwise = 
    let len = length arr
        remainder = (((val `mod` len) + len) `mod` len)
        (le, ri) = splitAt remainder arr
        in
            ri ++ le


-- Well, my implementation works infinitely, when the list is infinite, because it tries to calculate the length