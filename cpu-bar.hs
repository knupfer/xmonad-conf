module Main where

import System.IO
import Data.Char
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  stats <- newChan
  forkIO . forever $ do
    mem <- readFile "/proc/meminfo"
    cpu <- readFile "/proc/stat"
    net <- readFile "/proc/net/dev"
    writeChan stats (mem,cpu,net)
    threadDelay (5*10^5)
  (mem,cpu,net) <- (\(a, b, c) -> (getMem a, getCpu b, getNet c) ) . unzip3 <$> getChanContents stats
  stat     <- newMVar ""
  desktop <- newMVar ""
  input <- lines <$> getContents
  -- forkIO $
  --   mapM_ ((\x -> do
  --     putStr $ x ++ " | "
  --     d <- readMVar desktop
  --     putStrLn d
  --     hFlush stdout
  --     swapMVar stat x
  --     ) . concatMap numToBar) . slowDown $ replicate 500 (0,0,0) ++ zip3 cpu mem net
  forkIO $
     mapM_ (\x -> do

       d <- readMVar desktop
       putStr $ d ++ " | "
       putStrLn x
       hFlush stdout
       swapMVar stat x
       ) $ map barListToString$ scanl (addToList) barList (zip3 cpu mem net)
  mapM_ (\x -> do
         c <- readMVar stat
         swapMVar desktop x
         putStrLn $! x ++ " | " ++ c
         hFlush stdout)
   input

numToBar :: (Double,Double,Double) -> String
numToBar (cpu,mem,net) = concat [ "<fc=#"
                                , concatMap (numToHex . num)
                                  [ cpu
                                  , net
                                  , mem
                                  ]
                                 ,">=</fc>"]
  where num n = max 0 . round $ n*255
        numToHex n' = intToDigit (n' `div` 16) : [intToDigit (n' `mod` 16)]

slowDown :: [(Double,Double,Double)] -> [[(Double,Double,Double)]]
slowDown (_:xs) = slowDown' xs 30 : slowDown xs

slowDown' :: [(Double,Double,Double)] -> Int -> [(Double,Double,Double)]
slowDown' _  0 = []
slowDown' xs n = (addUp (\(x,_,_) -> x), addUp (\(_,x,_) -> x), addUp (\(_,_,x) -> x)) : slowDown' (drop n xs) (n-1)
  where addUp f = (/fromIntegral n) . sum . map f $ take n xs

----
getMem :: [String] -> [Double]
getMem = map ((\x -> 1-minimum x / maximum x) . map (read . head . drop 1) . filter ((`elem` ["MemTotal:", "MemAvailable:"]) . head) . map words . lines)

getCpu :: [String] -> [Double]
getCpu = (\(a,b) -> map (1-) $ zipWith (/) (diffs a) (diffs b)  ). unzip . map f
  where f = (\(_:user:nice:sys:idle:iowait:_) -> (read idle + read iowait, sum (map read [user, nice, sys, idle, iowait]))) . words

diffs :: Num c => [c] -> [c]
diffs ys = zipWith (-) (tail ys) ys

getNet :: [String] -> [Double]
getNet ss = (\xs -> zipWith (/) (0:xs) (scanl max 1 xs)) . map fromIntegral . diffs $ map getNet' ss

getNet' :: String -> Integer
getNet' s = sum . map (\ws -> read (ws !! 1) + read (ws !! 9)) . drop 2 $ words <$> lines s

barList :: [(Int,[(Double,Double,Double)])]
barList = zip (map (round. (** (1/4)).exp) [1..20]) (repeat [])

type BarList = [(Int,[(Double,Double,Double)])]

addToList :: BarList -> (Double,Double,Double) -> BarList
addToList ((n,xs) :xss) new | length xs >= n = (n,new:init xs): addToList xss (last xs)
                            | otherwise = (n,new:xs) : xss
addToList [] _ = []

barListToString :: BarList -> String
barListToString = concatMap (numToBar.(\(n,(a,b,c)) -> (a/n,b/n,c/n)) . (foldr (\(a,b,c) (n,(a',b',c')) -> (n+1,(a+a',b+b',c+c'))) (0,(0,0,0)) . snd))
