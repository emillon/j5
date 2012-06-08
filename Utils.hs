module Utils where

import Control.Applicative
import Control.Arrow
import Data.List

findAssoc :: Eq a => a -> [(a, b)] -> Maybe b
findAssoc n l =
  snd <$> find (\ (m, _) -> n == m) l

trimSpaces :: String -> String
trimSpaces = takeWhile $ \c -> c /= ' '

headTail :: [a] -> Maybe (a, [a])
headTail [] = Nothing
headTail (x:xs) = return (x, xs)

tplCallStr :: Int -> Int -> String -> String
tplCallStr begin end = drop begin
         >>> reverse
         >>> drop end
         >>> reverse

onLines :: (String -> String) -> String -> String
onLines f = unlines . map f . lines

