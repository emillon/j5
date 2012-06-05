import Data.Monoid
import Hakyll
import Hakyll.Core.Run (run)
import System.Directory
import Text.Pandoc.Shared

main :: IO ()
main = buildWiki

buildWiki :: IO ()
buildWiki = do
  writerOpts <- getWriterOpts
  _ <- hakyll (rules writerOpts)
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
