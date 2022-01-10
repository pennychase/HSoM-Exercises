module HSoM.Exercises.Chapter5 where

import HSoM
import Euterpea

-- Exercise 5.2
power :: (a -> a) -> Int -> a -> a
power f 1 x = f x
power f n x = f (power f (n - 1) x)

repeatNote :: Int -> Music Pitch -> Music Pitch
repeatNote i n = line $ power ([n] ++) i [n]

progression :: Int -> Int -> Music Pitch -> Music Pitch 
progression i step n = line $ power (\x -> [n] ++ map (transpose step) x) i [n]

-- Exercise 5.3

fix :: (a -> a) -> a
fix f = f (fix f)

remainder x y = fix remainder' x y
    where
        remainder' = \rec -> \a -> \b -> if a < b then a else rec (a - b) b

-- Exercise 5.4

apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs aps1 aps2 = [(ap1, ap2)| ap1 <- aps1, ap2 <- aps2, abs (ap1 - ap2) > 2 && abs (ap1 - ap2) < 8]

pairsToPitches :: [(AbsPitch, AbsPitch)] -> [Music Pitch]
pairsToPitches pairs =  [ toMP x :=: toMP y | (x, y) <- pairs]
    where
        df p = if odd p then en else sn         -- vary duration
        toMP abP = note (df abP) (pitch abP)    -- convert absPitch to a note, using df for duration

-- play $ line $ pairsToPitches $ apPairs ([60 .. 64] ++ [64, 63 .. 60]) ([62 .. 66]++[66,65 .. 62])
