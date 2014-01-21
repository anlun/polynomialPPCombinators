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

treeHeights = [6, 7, 8]
w = [25, 50, 100, 150]

cross l1 l2 = [(x, y) | x <- l1, y <- l2]

mapBURStest l = do
  trees <- return $ map (\h -> (fst (MapBURS.heightToDoc l h), h)) treeHeights
  a1 <- trees `deepseq` (return "")
  print a1
  printf "----------------------\nmapBURS\n----------------------\n"
  b <- foldl' (\f (w, (t, h)) -> do
                c <- f
                start <- getCurrentTime
                a <- return $ (++) c $ take 2 $ MapBURS.pretty w t
                print a
                stop  <- getCurrentTime
                printf "width: %d\nheight: %d\n" w h
                print $ diffUTCTime stop start
                printf "\n\n"
                return $ a
              ) (return "") $ cross w trees
  print b
 
clearUUtest l = do 
  uuTrees <- return $ map (\h -> (fst (ClearUU.heightToDoc l h), h)) treeHeights
  printf "----------------------\nclearUU\n----------------------\n"
  c <- foldl' (\f (w, (t, h)) -> do
                c <- f
                start <- getCurrentTime
                a <- UU.render t w
                print a
                stop  <- getCurrentTime
                printf "width: %d\nheight: %d\n" w h
                print $ diffUTCTime stop start
                printf "\n\n"
                return $ a
              ) (return ()) $ cross w uuTrees
  print c

hashMaptest l = do
  hashMapTrees <- return $ map (\h -> (fst (DocHashMap.heightToDoc l h), h)) treeHeights
  printf "----------------------\nhashMap\n----------------------\n"
  c <- foldl' (\f (w, (t, h)) -> do
                c <- f
                start <- getCurrentTime
                a <- return $ (++) c $ take 2 $ DocHashMap.pretty w t
                print a
                stop  <- getCurrentTime
                printf "width: %d\nheight: %d\n" w h
                print $ diffUTCTime stop start
                printf "\n\n"
                return $ a
              ) (return "") $ cross w hashMapTrees
  print c

main = do
  g <- getStdGen
  l <- return $ map (\v -> v:"") $ randomRs ('a', 'z') g
  mapBURStest l
  clearUUtest l
  hashMaptest l

