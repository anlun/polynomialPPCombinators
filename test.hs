import Data.Time
import Data.List
import System.IO
import Control.DeepSeq
import System.Random
import Text.Printf
import qualified MapBURS

import qualified ClearUU
import qualified UU.Pretty as UU

import qualified DocHashMap

treeHeights = [5, 6] --, 7]--[9, 10, 11]
w = [25, 50, 100, 150]

testNumber = 10
dividingF  = (/ 10)

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
                            start <- getCurrentTime
                            a <- return $ (++) c $ foldl (\f t ->
                                                           (++) f $ take 2 $ pretty w t
                                                         ) "" treeL
                            print a
                            stop <- getCurrentTime
                            printf "width: %d\nheight: %d\n" w h
                            printf "Average time: "
                            print $ dividingF $ diffUTCTime stop start
                            return a 
                          ) (return "") w
               return (c ++ a)
             ) (return "") trees 
  print b

testMapBURS = testF "mapBURS"    MapBURS.heightToDoc MapBURS.pretty
testClearUU = testF "clearUU"    ClearUU.heightToDoc (\w t -> UU.disp t w "")
testHashMap = testF "hashMap" DocHashMap.heightToDoc DocHashMap.pretty 

main = do
  hSetBuffering stdout NoBuffering
  g <- getStdGen
  l <- return $ map (\v -> v:"") $ randomRs ('a', 'z') g
  testHashMap l
  --testMapBURS l
  --testClearUU l

