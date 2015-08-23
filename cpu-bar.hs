module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Char
import           Data.List
import           System.IO

main :: IO ()
main = do
  stats <- newChan
  forkIO . forever $ (\[a,b,c] -> (a,b,c)) <$> getStats >>= writeChan stats
                     >> threadDelay (10^(6::Int))
  mapM_ ((>> hFlush stdout) . putStrLn) . getBarString =<< getChanContents stats
  where getStats = mapM readFile ["/proc/meminfo", "/proc/stat", "/proc/net/dev"]

getBarString :: [(String, String, String)] -> [String]
getBarString = map barListToString
               . scanl addToList barList
               . (\(a, b, c) -> zipWith3 BarElem (getMem a) (getCpu b) (getNet c))
               . unzip3

numToBar :: Int -> BarElem Int -> String
numToBar l (BarElem mem cpu net) = concat
  [ "<fc=#", concatMap numToHex [cpu, net, mem], ">"
  , replicate l '='
  , "</fc>"
  ]
  where numToHex n' = intToDigit (n' `div` 16) : [intToDigit (n' `mod` 16)]

getMem :: [String] -> [Double]
getMem = map ( (\x -> 1 - minimum x/maximum x)
             . map (read . head . drop 1)
             . filter ((`elem` ["MemTotal:", "MemAvailable:"]) . head)
             . map words
             . lines )

getCpu :: [String] -> [Double]
getCpu = (\(a, b) -> map (1-) $ zipWith (/) (diffs a) (diffs b)) . unzip . map f
  where f = (\(_:user:nice:sys:idle:iowait:_)
              -> ( read idle + read iowait
                 , sum (map read [user, nice, sys, idle, iowait])))
            . words

diffs :: Num c => [c] -> [c]
diffs ys = zipWith (-) (tail ys) ys

getNet :: [String] -> [Double]
getNet ss = (\xs -> zipWith (/) (0:xs) (scanl max 1 xs))
            . diffs
            $ map getNet' ss
  where getNet' s = sum
                  . map (\ws -> read (ws !! 1) + read (ws !! 9))
                  . drop 2
                  $ words
                  <$> lines s

barList :: BarList
barList = [(x, pure 0) | x <- map (round . (** (1/2.5)) . exp) [0.5,1..25]]

type BarList = [(Int, BarElem Double)]
data BarElem a = BarElem a a a deriving (Show, Eq)

instance Functor BarElem where
  fmap f (BarElem a b c) = BarElem (f a) (f b) (f c)

instance Applicative BarElem where
  pure a = BarElem a a a
  BarElem f g h <*> BarElem a b c =  BarElem (f a) (g b) (h c)

addToList :: BarList -> BarElem Double -> BarList
addToList [] _ = []
addToList ((n, x):xs) y = (n, (/n') .: (+) . ((n'-1)*) <$> x <*> y):addToList xs x
  where n' = fromIntegral n

barListToString :: BarList -> String
barListToString xs = concatMap (\l@(x:_) -> numToBar (length l) x)
                     . group
                     . map (colNum <$>)
                     . everySecond
                     $ zipWith mean xs (tail xs)
  where colNum = (`div`numberOfColors) . round . (*255) . max 0 . (fromIntegral numberOfColors*)
        everySecond (y:_:ys) = y : everySecond ys
        everySecond _ = []
        mean (_, a) (_, b) = (/2) .: (+) <$> a <*> b

numberOfColors :: Int
numberOfColors = 32

fromBarElem :: Num a => BarElem a -> (a,a,a)
fromBarElem (BarElem a b c) = (a, b, c)

infixr 9 .:

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
f .: g = (f .) . g
