{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens (over, (^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Data.List.Extra (lastDef, (!?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Lens (character, uri)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import Options.Applicative
import qualified Options.Applicative as O
import System.Exit (exitFailure)
import System.IO (IOMode (..), hGetContents, hPrint, openFile, stderr)
import qualified Toml as T (Result (..), decode)
import Toml.FromValue (FromValue (..), ParseTable, optKey, parseTableFromValue, reqKey, reqKeyOf)

data Snippet = Snippet
  { label :: Text, -- to be matched on completion prefix
    body :: Text, -- body of the snippet
    lang :: Maybe Text, -- language of the snippet
    doc :: Maybe Text -- helpful doc
  }

parseSnippet :: ParseTable Snippet
parseSnippet = do
  label <- reqKey "label"
  body <- reqKey "body"
  lang <- optKey "lang"
  doc <- optKey "doc"
  return Snippet {..}

instance FromValue Snippet where
  fromValue = parseTableFromValue parseSnippet

newtype Snippets = Snippets [Snippet]

instance FromValue Snippets where
  fromValue = parseTableFromValue (Snippets <$> reqKeyOf "snippets" fromValue)

emptyCompletionItem :: Text -> TextEdit -> CompletionItem
emptyCompletionItem label te =
  CompletionItem
    { _label = label,
      _labelDetails = Nothing,
      _kind = Nothing,
      _tags = Nothing,
      _detail = Nothing,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Just $ InL te,
      _textEditText = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _data_ = Nothing
    }

completionItemSnippet :: Text -> TextEdit -> CompletionItem
completionItemSnippet label te = (emptyCompletionItem label te) {_kind = Just CompletionItemKind_Snippet, _insertTextFormat = Just InsertTextFormat_Snippet, _insertTextMode = Just InsertTextMode_AdjustIndentation}

toCompletionItem :: Range -> Snippet -> CompletionItem
toCompletionItem range (Snippet {..}) =
  completionItemSnippet label (TextEdit range body)

handlers :: Handlers (LspM LspConfig)
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_ -> return (),
      notificationHandler SMethod_TextDocumentDidOpen $ \_ -> return (),
      notificationHandler SMethod_TextDocumentDidChange $ \_ -> return (),
      notificationHandler SMethod_TextDocumentDidSave $ \_ -> return (),
      notificationHandler SMethod_TextDocumentDidClose $ \_ -> return (),
      requestHandler SMethod_TextDocumentCompletion $ \req resp -> do
        let TRequestMessage _ _ _ (CompletionParams doc pos _workDone _partialResult _context) = req
        txt <- fmap virtualFileText <$> getVirtualFile (toNormalizedUri $ doc ^. uri)
        case txt of
          Just t ->
            let prefix = getCompletionPrefix t pos
                prefixLength = fromIntegral $ T.length prefix
                range = Range (over character (\c -> c - prefixLength) pos) pos
             in do
                  lift (hPrint stderr (T.append "completing: " prefix)) -- FIXME: remove debugging print
                  snippets <- snd <$> getConfig
                  lang <- language . fst <$> getConfig
                  let completions = (fmap (toCompletionItem range) . filterLanguage lang . filterSnippets prefix) snippets
                  resp $ Right (InL completions)
          Nothing -> resp $ Left (ResponseError (InR ErrorCodes_InternalError) "Failed to retrieve file content" Nothing)
    ]
  where
    getCompletionPrefix :: Text -> Position -> Text
    getCompletionPrefix txt (Position l c) =
      lastDef "" lineWords
      where
        lineWords = T.words (T.take (fromIntegral c + 1) line)
        mline = T.lines txt !? fromIntegral l
        line = fromMaybe "" mline

    filterLanguage :: Maybe String -> [Snippet] -> [Snippet]
    filterLanguage Nothing = id
    filterLanguage (Just l) = filter matchLang
      where
        matchLang (Snippet {..}) = case lang of
          Nothing -> True
          Just lan -> l == T.unpack lan
    filterSnippets :: Text -> [Snippet] -> [Snippet]
    filterSnippets prefix = filter (T.isPrefixOf prefix . label)

--------------------------------------------------------------------------------

type LspConfig = (Config, [Snippet])

data Config = Config {configPath :: FilePath, language :: Maybe String}

main :: IO Int
main = do
  config <- execParser $ info argParser fullDesc
  snippets <- loadSnippets $ configPath config
  runServer $
    ServerDefinition
      { defaultConfig = (config, snippets),
        configSection = "",
        parseConfig = \old _ -> Right old,
        onConfigChange = const $ return (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = \_caps -> handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions {optTextDocumentSync = Just $ TextDocumentSyncOptions (Just True) (Just TextDocumentSyncKind_Incremental) (Just True) (Just True) (Just $ InL True)}
      }
  where
    loadSnippets :: FilePath -> IO [Snippet]
    loadSnippets path = do
      h <- openFile path ReadMode
      contents <- hGetContents h
      let rSnippets = T.decode contents
      case rSnippets of
        T.Success _ (Snippets snippets) -> return snippets
        T.Failure m -> do
          forM_ m $ hPrint stderr
          exitFailure
    argParser :: O.Parser Config
    argParser =
      (Config <$> strOption (long "config" <> short 'c' <> value "$HOME/.config/lsnip/snippets.toml" <> help "Path to snippets configuration"))
        <*> option (Just <$> str) (long "lang" <> value Nothing)
