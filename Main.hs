import Data.Machine
import Data.List.Split
import Control.Monad.IO.Class (MonadIO(liftIO))

main :: IO ()
main = do
          -- putStrLn "Each Line Count"
          -- eachLineCount

          putStrLn "Compared with a known input, more or less\nPut in your fixed string:"
          fixedString <- getLine
          compareLineCounts fixedString

eachLineCount :: IO ()
eachLineCount = runT_ $ repeatedly (yield =<< liftIO getLine)
                     ~> countWords
                     ~> autoM print

-- countWords :: Process String Int
-- countWords = repeatedly $ do line <- await
--                              yield (length $ splitOn " " line)

countWords :: Process String Int
countWords = auto (length . splitOn " ")


--------------------------------------------------------

compareLineCounts :: String -> IO ()
compareLineCounts fixedString =
  runT_ $ tee (repeated fixedString ~> countWords) (ioInput ~> countWords) mergeInput
       ~> compareWords
       ~> autoM putStrLn

ioInput :: (MonadIO m) => SourceT m String
ioInput = repeatedly $ do
                        liftIO $ putStrLn "Enter your new line to compare: "
                        x <- liftIO getLine
                        yield x

mergeInput :: Tee a a (a,a)
mergeInput = repeatedly $ do
              x <- awaits L
              y <- awaits R
              yield (x, y)

compareWords :: (Ord a) => Process (a, a) String
compareWords = repeatedly $ do (x,y) <- await
                               yield $ case compare x y of
                                        GT -> "Greater Than"
                                        LT -> "Less Than"
                                        EQ -> "Equal To"

-- compareLineCounts :: String -> IO ()
-- compareLineCounts fixedString = 
--   runT_ $ (capR (repeated fixedString ~> countWords) $
--            addL (ioInput              ~> countWords)
--            mergeInput)
--        ~> compareWords
--        ~> autoM putStrLn

