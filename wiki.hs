import Data.Monoid
import Hakyll.Core.Configuration
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Core.Run
import Hakyll.Web.Page

main :: IO ()
main = buildWiki

buildWiki :: IO ()
buildWiki = do
  _ <- run conf rules
  return ()

conf :: HakyllConfiguration
conf = defaultHakyllConfiguration

rules :: RulesM ()
rules = do
  compileMarkdown

compileMarkdown :: Rules
compileMarkdown = do
  _ <- match (parseGlob "src/*") $ do
    route $ setExtension ".html"
    compile pageCompiler
  return ()
