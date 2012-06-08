module Templates where

import Control.Monad
import Control.Monad.RWS
import Data.List
import Data.List.Utils
import Data.Maybe

import Utils

expandTemplatesStr :: String -> String
expandTemplatesStr =
  expandInlineTemplatesStr . expandFilterTemplatesStr

expandInlineTemplatesStr :: String -> String
expandInlineTemplatesStr =
  onLines $ \s -> fromMaybe s $ do
    (t, args) <- findInlineTemplate s
    tf <- findTemplateNamed t -- TODO emit error on failure
    return $ tf args

-- [!name <<
-- template input
-- >>]
expandFilterTemplatesStr :: String -> String
expandFilterTemplatesStr is =
  unlines ls -- TODO onLines ?
    where
      ((), _, ls) = runRWS m () s0
      m = mapM_ tfHandleLine $ lines is
      s0 = TFState { tfsActive = Nothing
                   , tfsFilterOutput = []
                   }

--  [!name arg0 arg1]
-- TODO
--   - what if it's not the only thing on current line ?
findInlineTemplate :: String -> Maybe (String, [String])
findInlineTemplate s = do
  guard $ "[!" `isPrefixOf` s
  guard $ "]"  `isSuffixOf` s
  let tplCall = split " " $ tplCallStr 2 1 s
  headTail tplCall

findTemplateNamed :: String -> Maybe MWTemplate
findTemplateNamed n =
  findAssoc n allTemplates

data TFState = TFState { tfsActive :: Maybe String
                       , tfsFilterOutput :: [String]
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

findFilterTemplateNamed :: String -> Maybe TFTemplate
findFilterTemplateNamed n =
  findAssoc n allTFTemplates

tfExtractStart :: String -> Maybe String
tfExtractStart s = do
  guard $ "[!" `isPrefixOf` s
  guard $ "<<" `isSuffixOf` s
  return $ trimSpaces $ tplCallStr 2 2 s

tfExtractEnd :: String -> Bool
tfExtractEnd s =
  s == ">>]"
