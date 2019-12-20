--runghc test.hs

import Data.Bool
import Data.Semigroup
import Data.Monoid

  
blackjack :: Int -> Int -> Int -> String
blackjack a b c = winbust $ over a b c


over :: Int -> Int -> Int -> Bool
over a b c = n <= 21
  where n = a + b + c


winbust :: Bool -> String
winbust True = "win"
winbust False = "bust"

-------------------------

palidrome :: String -> Int
palidrome xs = n `div` 2
  where n = sum $ falselist xs

falselist :: String -> [Int]
falselist xs = (map judgefalse) $ firstlast xs

judgefalse :: (Char, Char) -> Int
judgefalse (a, b) = case a == b of
 False -> 1
 True -> 0

firstlast ::String -> [(Char, Char)]
firstlast xs = zip (firstset xs n) (lastset xs n)
  where n = length xs

firstset :: String -> Int -> String
firstset xs n  = take (n-1 `div` 2) xs

lastset :: String -> Int -> String
lastset xs n  =  take (n-1 `div` 2) $ reverse xs




main = do
  print $ blackjack 1 2 3
  print $ blackjack 12 12 12
  print $ palidrome "じてんしゃ"
  print $ palidrome "aaabbbaac"
  print $ palidrome "aba"
  print $ falselist "meidaiigakuka"

