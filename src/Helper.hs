module Helper where

import Control.Monad (replicateM)
import qualified System.Random as SR
import Data.Text (Text, pack)

alphaNum :: String
alphaNum = ['a'..'z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return $ xs !! randomDigit

shortUrlGen :: IO [Char]
shortUrlGen =
  replicateM 7 $ randomElement alphaNum

tshow :: Show a => a -> Text
tshow = pack . show
