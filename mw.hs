import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Data.List
import Data.List.Utils
import Data.Maybe
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

expandTemplatesStr :: String -> String
expandTemplatesStr =
  expandInlineTemplatesStr . expandFilterTemplatesStr

data TFState = TFState { tfsActive :: Maybe String
                       , tfsFilterOutput :: [String]
                       }

-- [!name <<
-- template input
-- >>]
-- TODO implement
expandFilterTemplatesStr :: String -> String
expandFilterTemplatesStr is =
  unlines ls
    where
      ((), _, ls) = runRWS m () s0
      m = mapM_ tfHandleLine $ lines is
      s0 = TFState { tfsActive = Nothing
                   , tfsFilterOutput = []
                   }

type TFM a = RWS () [String] TFState a

tfHandleLine :: String -> TFM ()
tfHandleLine s = do
  activeTpl <- gets tfsActive
  case (isStart, isEnd, activeTpl) of
    -- normal line
    (Nothing, False, Nothing) -> tell [s]
    -- tf start
    (Just tpl, False, Nothing) -> tfSetActive tpl
    -- tf end
    (Nothing, True, Just t) -> tfFlush t
    -- output through filter
    (Nothing, False, Just _) -> tfsAppendFilter s

    (Just _, True, _) -> error "tfHandleLine : start and end on same line"
    (Nothing, True, Nothing) -> error "tfHandleLine : end without start"
    (Just _, False, Just _) -> error "tfHandleLine : nested templates"

    where
      isStart = tfExtractStart s
      isEnd   = tfExtractEnd s

      -- meh, use lenses ?
      tfSetActive tpl = modify $ \tfs -> tfs { tfsActive = Just tpl }
      tfSetInactive   = modify $ \tfs -> tfs { tfsActive = Nothing }
      tfsAppendFilter os =
        modify $ \tfs ->
          tfs { tfsFilterOutput = tfsFilterOutput tfs ++ [os] }
      tfsClearFilter =
        modify $ \tfs ->
          tfs { tfsFilterOutput = [] }

      tfFlush :: String -> TFM ()
      tfFlush tpl = do
        let f = fromMaybe (error $ "Cannot find TF " ++ show tpl) $ findFilterTemplateNamed tpl
        out <- gets tfsFilterOutput
        tell $ f out
        tfsClearFilter
        tfSetInactive

tfExtractStart :: String -> Maybe String
tfExtractStart s = do
  guard $ "[!" `isPrefixOf` s
  guard $ "<<" `isSuffixOf` s
  return $ trimSpaces $ tplCallStr 2 2 s

trimSpaces :: String -> String
trimSpaces = takeWhile $ \c -> c /= ' '

tfExtractEnd :: String -> Bool
tfExtractEnd s =
  s == ">>]"

tplCallStr :: Int -> Int -> String -> String
tplCallStr begin end = drop begin
         >>> reverse
         >>> drop end
         >>> reverse

expandInlineTemplatesStr :: String -> String
expandInlineTemplatesStr =
  onLines $ \s -> fromMaybe s $ do
    (t, args) <- findInlineTemplate s
    tf <- findTemplateNamed t -- TODO emit error on failure
    return $ tf args

onLines :: (String -> String) -> String -> String
onLines f = unlines . map f . lines

findAssoc :: Eq a => a -> [(a, b)] -> Maybe b
findAssoc n l =
  snd <$> find (\ (m, _) -> n == m) l

findTemplateNamed :: String -> Maybe MWTemplate
findTemplateNamed n =
  findAssoc n allTemplates

findFilterTemplateNamed :: String -> Maybe TFTemplate
findFilterTemplateNamed n =
  findAssoc n allTFTemplates

--  [!name arg0 arg1]
-- TODO
--   - what if it's not th eonly thing on current line ?
findInlineTemplate :: String -> Maybe (String, [String])
findInlineTemplate s = do
  guard $ "[!" `isPrefixOf` s
  guard $ "]"  `isSuffixOf` s
  let tplCall = split " " $ tplCallStr 2 1 s
  headTail tplCall

headTail :: [a] -> Maybe (a, [a])
headTail [] = Nothing
headTail (x:xs) = return (x, xs)

type MWTemplate = [String] -> String

allTemplates :: [(String, MWTemplate)]
allTemplates = [("test", tplTest)]

tplTest :: MWTemplate
tplTest args =
  "**test template** : arguments = " ++ show args

type TFTemplate = [String] -> [String]

tplCatn :: TFTemplate
tplCatn =
  zipWith (\ n s -> show n ++ " " ++ s) [1::Integer ..]

allTFTemplates :: [(String, TFTemplate)]
allTFTemplates = [("numlines", tplCatn)]
