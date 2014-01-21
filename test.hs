import Data.Time
import System.Random
import qualified MapBURS

treeHeights = [6]--, 7, 8]
w = [25, 50, 100, 150]

cross l1 l2 = [(x, y) | x <- l1, y <- l2]

main = do
  start <- getCurrentTime
  g <- getStdGen

  l <- return $ map (\v -> v:"") $ randomRs ('a', 'z') g
  trees <- return $ map (fst . (MapBURS.heightToDoc l)) treeHeights 
  results <- return $ map (uncurry MapBURS.pretty) $ cross w trees
  print $ results

  stop <- getCurrentTime
  print $ diffUTCTime stop start
