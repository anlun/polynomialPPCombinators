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

treeHeights = [6, 7] --, 8, 9, 10, 11]
widths = [25]--[25, 50, 100]--]--, 150]

testNumber = 1 :: Integer
dividingF  = \x -> x `div` testNumber

--fromPicoSeconds :: Integer -> Double
fromPicoSeconds = \x -> (fromIntegral x) / (10.0 ^ 12)

cross l1 l2 = [(x, y) | y <- l2, x <- l1]

treesF heightToDoc treeHeights l =
  map (\h -> let htd         = \l -> heightToDoc l h
                 swapAndCons = \xs (t, nl) -> (nl, t:xs)
                 prFold      = foldl (\(l, xs) _ -> (swapAndCons xs) (htd l))
                 pr          = prFold (l, []) [1..testNumber] in
             (h, snd pr)
  ) treeHeights 

testF s heightToDoc pretty l =
  let trees = treesF heightToDoc treeHeights l in
  foldl (\midRes (w, (h, treeL)) ->
    --let prettyL = deepseq treeL (map (pretty w) treeL) in
    let prettyL = map (pretty w) treeL in
    do
    r <- midRes
    printf "width: %d\nheight: %d\n" w h
    printf "Average time: "
    start <- getCPUTime
    stop <- deepseq prettyL getCPUTime
    print $ fromPicoSeconds $ dividingF $ stop - start
    return ()
  ) (return () :: IO ()) (cross widths trees)

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
  putStrLn "----------\nHashMap\n-----------\n\n"
  testHashMap l
  putStrLn "----------\nMapBURS\n-----------\n\n"
  testMapBURS l
  putStrLn "----------\nClearUU\n-----------\n\n"
  testClearUU l
