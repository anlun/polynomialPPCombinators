import Data.Time
import Data.List
import System.IO
import Control.DeepSeq
import System.Random
import Text.Printf
import qualified MapBURS

treeHeights = [5, 6, 7]--, 7, 8]
w = [25, 50, 100, 150]

cross l1 l2 = [(x, y) | x <- l1, y <- l2]

main = do
  g <- getStdGen
  l <- return $ map (\v -> v:"") $ randomRs ('a', 'z') g
  trees <- return $ map (\h -> (fst (MapBURS.heightToDoc l h), h)) treeHeights
  a1 <- trees `deepseq` (return "")
  print a1
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
  print "hello"
