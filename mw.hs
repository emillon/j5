import Control.Monad
import Hakyll
import System.Directory
import Text.Pandoc.Shared

main :: IO ()
main = buildWiki

buildWiki :: IO ()
buildWiki = do
  writerOpts <- getWriterOpts
  hakyll $ rules writerOpts

getWriterOpts :: IO WriterOptions
getWriterOpts = do
  htmlTemplate <- readFile "templates/html.default"
  root <- getCurrentDirectory
  let extraVars = [ ("css", root ++ "/css/style.css")
                  , ("wikiroot", root ++ "/wiki/")
                  ]
  return $ addVars extraVars
         $ defaultWriterOptions { writerStandalone = True
                                , writerTemplate = htmlTemplate
                                , writerTableOfContents = True
                                }

addVars :: [(String, String)] -> WriterOptions -> WriterOptions
addVars vars opts =
  opts { writerVariables = vars ++ writerVariables opts }

conf :: HakyllConfiguration
conf = defaultHakyllConfiguration

rules :: WriterOptions -> RulesM ()
rules = do
  compileMarkdown

compileMarkdown :: WriterOptions -> Rules
compileMarkdown wo =
  void $ do
    match (parseGlob "wiki/*") $ do
    route $ setExtension ".html"
    compile (pdcCompiler wo)

pdcCompiler :: WriterOptions -> Compiler Resource (Page String)
pdcCompiler writerOpts =
  pageCompilerWith defaultHakyllParserState writerOpts
