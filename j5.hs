import Control.Arrow
import Control.Monad
import Hakyll
import System.Directory
import Text.Pandoc.Shared

import Templates

main :: IO ()
main = buildWiki

buildWiki :: IO ()
buildWiki = do
  writerOpts <- getWriterOpts
  hakyll $ rules writerOpts

getWriterOpts :: IO WriterOptions
getWriterOpts = do
  htmlTemplate <- readFile "templates/html.default"
  cwd <- getCurrentDirectory
  let root = cwd ++ "/_site/"
      extraVars = [ ("css", root ++ "/css/style.css")
                  , ("wikiroot", root ++ "/wiki/")
                  ]
  return $ addVars extraVars
         $ defaultWriterOptions { writerStandalone = True
                                , writerTemplate = htmlTemplate
                                }

addVars :: [(String, String)] -> WriterOptions -> WriterOptions
addVars vars opts =
  opts { writerVariables = vars ++ writerVariables opts }

conf :: HakyllConfiguration
conf = defaultHakyllConfiguration

rules :: WriterOptions -> RulesM ()
rules opts = do
  copyCss
  compileMarkdown opts

compileMarkdown :: WriterOptions -> Rules
compileMarkdown wo =
  void $ match (parseGlob "wiki/**") $ do
    route $ setExtension ""
    compile $ readPageCompiler
          >>> expandTemplatesCompiler
          >>> pdcCompiler wo

pdcCompiler :: WriterOptions -> Compiler (Page String) (Page String)
pdcCompiler writerOpts =
      addDefaultFields
  >>> arr applySelf
  >>> pageReadPandocWith defaultHakyllParserState
  >>> arr (fmap (writePandocWith writerOpts))

copyCss :: Rules
copyCss =
  void $ match (parseGlob "css/*") $ do
    route idRoute
    compile compressCssCompiler

expandTemplatesCompiler :: Compiler (Page String) (Page String)
expandTemplatesCompiler =
  arr $ \p ->
    Page { pageMetadata = pageMetadata p
        , pageBody = expandTemplatesStr $ pageBody p
        }
