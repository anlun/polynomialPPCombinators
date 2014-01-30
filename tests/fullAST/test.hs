import Data.Time
import Data.List
import System.IO
import Control.DeepSeq
import System.Random
import Text.Printf

import qualified AltPretty
import qualified AltPrettyTest

import qualified UUTest
import qualified UU.Pretty as UU

import qualified Pretty
import qualified PrettyTest

treeHeights = [7, 8] --, 7]--[9, 10, 11]
w = [25, 50, 100, 150]

testNumber = 1
dividingF  = (/ 1)

cross l1 l2 = [(x, y) | y <- l2, x <- l1]

testF s heightToDoc pretty l = do
  trees <- return $ map (\h ->
                          snd $ foldl(\(l, (_, xs)) _ ->
                                       let (t, nl) = heightToDoc h l in
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

testHashMap = testF "lib"       PrettyTest.heightToDoc    Pretty.pretty 
testMapBURS = testF "altLib" AltPrettyTest.heightToDoc AltPretty.pretty
testClearUU = testF "UU"            UUTest.heightToDoc (\w t -> UU.disp t w "")

main = do
  hSetBuffering stdout NoBuffering
  g <- getStdGen
  l <- return $ map (\v -> v:"") $ randomRs ('a', 'z') g
  --testHashMap l
  testMapBURS l
  --testClearUU l
