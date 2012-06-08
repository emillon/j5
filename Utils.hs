module Utils ( onLines
             , biDrop
             , unCons
             , findAssoc
             , trimSpaces
             ) where

import Control.Applicative
import Control.Arrow
import Data.List

findAssoc :: Eq a => a -> [(a, b)] -> Maybe b
findAssoc n l =
  snd <$> find (\ (m, _) -> n == m) l

trimSpaces :: String -> String
trimSpaces = takeWhile $ \c -> c /= ' '

unCons :: [a] -> Maybe (a, [a])
unCons [] = Nothing
unCons (x:xs) = return (x, xs)

biDrop :: Int -> Int -> String -> String
biDrop begin end =
      drop begin
  >>> reverse
  >>> drop end
  >>> reverse

onLines :: (String -> String) -> String -> String
onLines f = unlines . map f . lines

