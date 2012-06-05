import Data.Monoid
import Hakyll.Core.Configuration
import Hakyll.Core.Rules
import Hakyll.Core.Run

main :: IO ()
main = buildWiki

buildWiki :: IO ()
buildWiki = do
  _ <- run conf rules
  return ()

conf :: HakyllConfiguration
conf = defaultHakyllConfiguration

rules :: RulesM ()
rules = return ()
