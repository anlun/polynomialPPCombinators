import Data.Time
import Data.List
import System.IO
import Control.DeepSeq
import System.CPUTime
import Text.Printf

import qualified AltPretty
import qualified AltPrettyTest

import qualified UUTest
import qualified UU.Pretty as UU

import qualified Pretty
import qualified PrettyTest

treeHeights = [6, 7, 8, 9, 10, 11]
w = [25]--[25, 50, 100]--]--, 150]

testNumber = 1
dividingF  = \x -> x `div` 1

--fromPicoSeconds :: Integer -> Double
fromPicoSeconds = \x -> (fromIntegral x) / (10.0 ^ 12)

cross l1 l2 = [(x, y) | y <- l2, x <- l1]

testF s heightToDoc pretty l = do
  trees <- return $ map (\h ->
                          snd $ foldl(\(l, (_, xs)) _ ->
                                       let (t, nl) = heightToDoc l h in
                                       (nl, (h, t:xs))
                                     ) (l, (h, [])) [1..testNumber]
                        ) treeHeights
  --printf "----------------------\n%s\n----------------------\n" s
  b <- foldl (\f (h, treeL) -> do
               --- Fix tree height and tree list
               c <- f
               a <- foldl (\f w -> do
                            --- Fix width
                            c <- f
                            start <- getCPUTime
                            a <- return $ (++) c $ foldl (\f t ->
                                                           (++) f $ take 2 $ pretty w t
                                                         ) "" treeL
                            print a
                            stop <- getCPUTime
                            printf "width: %d\nheight: %d\n" w h
                            printf "Average time: "
                            print $ fromPicoSeconds $ dividingF $ stop - start
                            return a 
                          ) (return "") w
               return (c ++ a)
             ) (return "") trees 
  print b

testHashMap = testF "lib"       PrettyTest.heightToDoc    Pretty.pretty 
testMapBURS = testF "altLib" AltPrettyTest.heightToDoc AltPretty.pretty
testClearUU = testF "UU"            UUTest.heightToDoc (\w t -> UU.disp t w "")

list :: Int -> [String]
list 0 = map (\v -> v:"") ['a'..'z']
list n = [x ++ xs | x <- list 0, xs <- list (n-1)]

symbolList = (list 0) ++ symbolList

main = do
  hSetBuffering stdout NoBuffering
  l <- return symbolList
  --putStrLn "----------\nHashMap\n-----------\n\n"
  --testHashMap l
  putStrLn "----------\nMapBURS\n-----------\n\n"
  testMapBURS l
  putStrLn "----------\nClearUU\n-----------\n\n"
  testClearUU l
