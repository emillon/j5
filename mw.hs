import Data.Monoid
import Hakyll.Core.Compiler
import Hakyll.Core.Configuration
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Resource
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Core.Run
import Hakyll.Web.Page
import Hakyll.Web.Pandoc
import System.Directory
import Text.Pandoc.Shared

main :: IO ()
main = buildWiki

buildWiki :: IO ()
buildWiki = do
  writerOpts <- getWriterOpts
  _ <- run conf (rules writerOpts)
  return ()

getWriterOpts :: IO WriterOptions
getWriterOpts = do
  htmlTemplate <- readFile "templates/html.default"
  root <- getCurrentDirectory
  let extraVars = [ ("css", root ++ "/css/style.css")
                  , ("wikiroot", root ++ "/wiki/")
                  ]
  return $ defaultWriterOptions { writerStandalone = True
                                , writerTemplate = htmlTemplate
                                , writerTableOfContents = True
                                , writerVariables = extraVars ++ writerVariables defaultWriterOptions
                                }

conf :: HakyllConfiguration
conf = defaultHakyllConfiguration

rules :: WriterOptions -> RulesM ()
rules = do
  compileMarkdown

compileMarkdown :: WriterOptions -> Rules
compileMarkdown wo = do
  _ <- match (parseGlob "wiki/*") $ do
    route $ setExtension ".html"
    compile (pdcCompiler wo)
  return ()

pdcCompiler :: WriterOptions -> Compiler Resource (Page String)
pdcCompiler writerOpts =
  pageCompilerWith defaultHakyllParserState writerOpts
