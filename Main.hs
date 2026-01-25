{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, (.:?), (.!=))
import Data.Bifunctor
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Options.Applicative
import Options.Applicative.Help.Pretty hiding ((</>))
import System.Directory
import System.FilePath
import System.IO
import System.Process
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Autograder
  = Autograder
    { tests :: [AutograderTest]
    }
  deriving (Generic)

instance ToJSON Autograder

data AutograderTest
  = AutograderTest
    { score     :: !Double
    , max_score :: !Double
    , name      :: !Text
    , output    :: !Text
   }
  deriving (Generic)

instance ToJSON AutograderTest

data DialogueOpts
  = DialogueOpts
    { dopts_max_score    :: !Double
    -- ^ The number of points that this dialogue is worth.
    , dopts_name         :: !(Maybe Text)
    -- ^ The name of this test.
    , dopts_extra_args   :: !String
    -- ^ Extra command-line arguments to append to the command-line
    -- arguments provided at the command-line.
    , dopts_input_prefix :: !Text
    -- ^ The symbol which should be matched to identify that a line of
    -- the dialogue is part of the input.
    , dopts_timeout_ms   :: !Int
    -- ^ The number of milliseconds to wait for the executable to
    -- deliver a line of output before failing.
    }
    deriving (Show)

instance FromJSON DialogueOpts where
  parseJSON (J.Object v) = DialogueOpts
    <$> v .:? "max_score"    .!= 1
    <*> v .:? "name"
    <*> v .:? "extra_args"   .!= ""
    <*> v .:? "input_prefix" .!= ">"
    <*> v .:? "timeout_ms"   .!= 1000

  parseJSON invalid =
    J.prependFailure "parsing Coord failed, "
    (J.typeMismatch "Object" invalid)

data Section = InputSection  !Lines
             | OutputSection !Lines
  deriving (Show)

data Dialogue
  = Dialogue
    { filepath :: !FilePath
    -- ^ The filepath where we found this dialogue.
    , opts     :: !DialogueOpts
    -- ^ Top-level options having to do with this dialogue.
    , sections :: ![Section]
    }
    deriving (Show)

type Lines = [Text]

-- | Try to parse the given line as an input line, returning @Just l@
-- where @l@ is the rest of the input line if it could be parsed as
-- such, and @Nothing@ if the line isn't an input line.
asInputLine :: DialogueOpts -> Text -> Maybe Text
asInputLine DialogueOpts { dopts_input_prefix } =
  T.stripPrefix dopts_input_prefix

-- | Apply the function to the input list, splitting the input list
-- into two parts: The longest prefix of @a@s which could be converted
-- to @Just b@s, and the rest of the @a@s.
spanMaybes :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybes _ [] = ([], [])
spanMaybes f (a:as) = case f a of
  Just b  -> first (b:) (spanMaybes f as)
  Nothing -> ([], a:as)

-- | Return the longest common prefix of all of the strings.
longestCommonPrefix :: [Text] -> Text
longestCommonPrefix [] = ""
longestCommonPrefix ss@(s:_)
  | any T.null ss = ""
  | all ((==p) . T.take 1) ss =
    p <> longestCommonPrefix (map (T.drop 1) ss)
  | otherwise = ""
  where
    p = T.take 1 s

-- | Like `longestCommonPrefix`, except only returns prefixes which
-- are composed only of whitespace characters.
longestCommonSpacePrefix :: [Text] -> Text
longestCommonSpacePrefix = T.takeWhile isSpace . longestCommonPrefix

-- | Remove the whitespace from the beginning of lines, so long as
-- each line has the same amount of whitespace.
removeLeadingWhitespace :: Lines -> Lines
removeLeadingWhitespace ls = map (T.drop n) ls
  where n = T.length $ longestCommonSpacePrefix ls

-- | Parse an input section of the dialogue, which is identified by a
-- sequence of lines starting with the prefix character.
--
-- On success, return the stdin section (omitting the prefix), and the
-- remainder of the input which was not parsed.
parseInputSection :: DialogueOpts -> Lines -> Except String (Lines, Lines)
parseInputSection opts ls
  | null inpt = throwError "Expected input section to have at least one line of input."
  | otherwise = pure (removeLeadingWhitespace inpt, rest)
  where
    (inpt, rest) = spanMaybes (asInputLine opts) ls

-- | Parse an output section of the dialogue, which is identified by a
-- sequence of lines, not starting with the prefix character.
parseOutputSection :: DialogueOpts -> Lines -> Except String (Lines, Lines)
parseOutputSection opts ls
  | null otpt = throwError "Expected output section to have at least one line of output."
  | otherwise = pure (otpt, rest)
  where
    (otpt, rest) = span (isNothing . asInputLine opts) ls

-- | Return @True@ if the line could be interpreted as a separator,
-- and @False@ otherwise.
asSeparator :: Text -> Bool
asSeparator = T.all (=='-') . T.strip

parseDialoguePrefix :: Lines -> Except String (DialogueOpts, Lines)
parseDialoguePrefix ls =
  let (pfx, rest) = break asSeparator ls
  in case J.eitherDecodeStrictText (T.unlines pfx) of
       Left err ->
         throwError ("Failed to parse dialogue options:\n" ++ err)
       Right opts -> pure (opts, drop 1 rest)

-- | Parse a dialogue given its @FilePath@, and its @Text@ contents.
parseDialogue :: FilePath -> Text -> Except String Dialogue
parseDialogue filepath src = do
  let rest0 = T.lines src
  (opts, rest1) <- parseDialoguePrefix rest0
  sections <- case rest1 of
    []    -> pure []
    rest2@(l0:_)
      | isJust (asInputLine opts l0) -> expectInputSection opts rest2
      | otherwise -> expectOutputSection opts rest2
  pure Dialogue { opts, filepath, sections }

  where expectInputSection opts rest = do
          (ls, rest') <- parseInputSection opts rest
          let s = InputSection ls
          case rest' of
            [] -> pure [s]
            rest'' -> (s:) <$> expectOutputSection opts rest''

        expectOutputSection opts rest = do
          (ls, rest') <- parseOutputSection opts rest
          let s = OutputSection ls
          case rest' of
            [] -> pure [s]
            rest'' -> (s:) <$> expectInputSection opts rest''

decorateWithSeverity :: Int -> String -> String
decorateWithSeverity s t = case s of
  0 -> "\x1b[31m" ++ t ++ "\x1b[0m"
  1 -> "\x1b[33m" ++ t ++ "\x1b[0m"
  _ -> t

logWithSeverity :: MonadIO m => Int -> Int -> String -> m ()
logWithSeverity severity verbosity msg
  | severity <= verbosity =
    liftIO $ putStrLn (decorateWithSeverity severity msg)
  | otherwise = pure ()

logInfo :: MonadIO m => Int -> String -> m ()
logInfo = logWithSeverity 2

logWarn :: MonadIO  m => Int -> String -> m ()
logWarn = logWithSeverity 1

logErr :: MonadIO  m => Int -> String -> m ()
logErr = logWithSeverity 0

waitForResponse :: Int -> Handle -> IO (Maybe Text)
waitForResponse initialDelay hdl = do
  let go0 delay = do
        rdy <- hWaitForInput hdl delay
        if rdy
          -- On recursive calls, wait only 100 milliseconds for the next character.
          then Just <$> go 100
          else pure Nothing

      go delay = do
        rdy <- hWaitForInput hdl delay
        if rdy
          then do
          c <- hGetChar hdl
          if c == '\n'
            then pure ""
            else (c:) <$> go delay
          else pure ""

  -- Wait upto 1 second for a response.
  fmap T.pack <$> go0 initialDelay

annotateFirstInputSection :: [Section] -> [(Section, Bool)]
annotateFirstInputSection [] = []
annotateFirstInputSection (s@InputSection {}:rest) =
  (s, True):map (,False) rest
annotateFirstInputSection (s@OutputSection {}:rest) =
  (s, False):annotateFirstInputSection rest

annotateLastInputSection :: [Section] -> [(Section, Bool)]
annotateLastInputSection = reverse . annotateFirstInputSection . reverse

evaluateDialogue :: Options -> Dialogue -> ExceptT Text IO ()
evaluateDialogue Options {..} Dialogue {..} = do
  let args = words extra_options ++ words (dopts_extra_args opts)

  logInfo verbosity $ "Running " ++ target ++ " with arguments: " ++ intercalate ", " args

  -- Put stdout and stderr on the same stream
  (readEnd, w) <- liftIO createPipe
  liftIO $ hSetBuffering readEnd NoBuffering

  (Just writeEnd, _, _, _) <- liftIO $ createProcess (proc target args)
                              { cwd = target_cwd
                              , std_out = UseHandle w
                              , std_err = UseHandle w
                              , std_in  = CreatePipe }

  liftIO $ hSetBuffering writeEnd NoBuffering

  forM_ (annotateLastInputSection sections) $ \case
    (OutputSection ls, _) -> do
      forM_ (zip [(0::Int)..] ls) $ \(n, expected) -> do
        maybeActual <- liftIO $ catch (waitForResponse (dopts_timeout_ms opts) readEnd)
          (\(e :: IOError) -> pure (Just $ T.pack $ show e))

        actual <- case maybeActual of
          Just l -> pure l
          Nothing -> throwError
            $ mconcat [ "The executable took too long (> "
                      , T.pack (show (dopts_timeout_ms opts))
                      , " milliseconds) delivering line "
                      , T.pack (show n) <> " of block:\n"
                      , T.unlines ls ]
        logInfo verbosity ("Got: " ++ show actual)
        when (T.strip expected /= T.strip actual) $ do
          throwError $ mconcat
            [ "On line " <> T.pack (show n) <> " of block:\n"
            , T.unlines ls
            , "Expected:\n"
            , expected <> "\n"
            , "But got:\n"
            , actual ]
    (InputSection ls, isLast) -> do
      forM_ ls $ \l -> do
        liftIO $ T.hPutStrLn writeEnd l
        logInfo verbosity ("Wrote: " ++ show l)

      when isLast $ liftIO $ hClose writeEnd

evaluateDialogueIntoTest :: Options -> Dialogue -> IO AutograderTest
evaluateDialogueIntoTest o d = do
  e <- runExceptT $ evaluateDialogue o d
  pure $ case e of
    Left err -> mkTestResult False err
    Right () -> mkTestResult True "passed"
  where
    mkTestResult :: Bool -> Text -> AutograderTest
    mkTestResult passed output =
      AutograderTest
      { name
      , score = if passed then maxScore else 0
      , output
      , max_score = maxScore
      }

    -- | The name of the dialogue. If a name is provided in the
    -- dialogue options, we use that. Otherwise, we use the basename
    -- of the dialogue file.
    name :: Text
    name = fromMaybe
      (T.pack $ takeBaseName $ filepath d)
      (dopts_name (opts d))

    maxScore :: Double
    maxScore = dopts_max_score (opts d)

hasDialogueExt :: FilePath -> Bool
hasDialogueExt = (==".dialogue") . takeExtension

parseDialogueFromFile :: Options -> FilePath -> IO (Maybe Dialogue)
parseDialogueFromFile Options { verbosity } path = do
  logInfo verbosity ("Parsing dialogue at " ++ path)
  src <- liftIO $ T.readFile path
  let res = runExcept (parseDialogue path src)
  case res of
    Left err ->
      logErr verbosity ("Could not parse " ++ path ++ ":\n" ++ err) $> Nothing
    Right d ->
      pure (Just d)

discoverDialoguesAtPath :: Options -> FilePath -> IO [Dialogue]
discoverDialoguesAtPath o@Options { verbosity } path = do
  logInfo verbosity ("Searching " ++ path)

  isFile <- liftIO $ doesFileExist path
  isDir  <- liftIO $ doesDirectoryExist path

  let ds
        | isFile = do
            d <- parseDialogueFromFile o path
            pure [d]
        | isDir = do
            candidates <- filter hasDialogueExt <$> liftIO (listDirectory path)
            mapM (parseDialogueFromFile o . (path </>)) candidates
        | otherwise = pure []

  catMaybes <$> ds

-- | From a list of search paths, produce a list of @Dialogue@s to
-- evaluate.
discoverDialogues :: Options -> [FilePath] -> IO [Dialogue]
discoverDialogues o paths = concat <$> mapM (discoverDialoguesAtPath o) paths

data Options
  = Options
    { target        :: !FilePath
    , target_cwd    :: !(Maybe FilePath)
    , extra_options :: !String
    , out_path      :: !(Maybe FilePath)
    , search_paths  :: ![FilePath]
    , verbosity     :: !Int
    }

optionsParser :: Parser Options
optionsParser =
  Options
  <$> strArgument ( help "A path which points to the target executable."
                    <> metavar "TARGET" )
  <*> optional ( strOption ( long "cwd"
                             <> short 'C'
                             <> metavar "PATH"
                             <> help "The working directory to run the TARGET executable." ) )
  <*> (fromMaybe "" <$> optional
       ( strOption ( long "extra-arguments"
                     <> short 'a'
                     <> help "Extra argument to pass to the target executable." ) ) )
  <*> optional ( strOption ( long "output"
                             <> short 'o'
                             <> metavar "PATH"
                             <> help "The path to write out the test result JSON." ) )
  <*> many ( strOption ( long "path"
                         <> short 'p'
                         <> help "The path to a directory or file. \
                                 \If the path points to a directory, \
                                 \it will be searched for dialogue files. \
                                 \If it points to a file, the file will be \
                                 \interpreted as a test dialogue." ) )
  <*> option auto ( long "verbosity"
                    <> short 'v'
                    <> value 1
                    <> showDefault
                    <> help "How verbose to be. The options are 0 (show only errors), \
                            \1 (show errors and warnings), and 2 (show all messages)" )

pgraph :: String -> Doc
pgraph = fillSep . map pretty . words

main :: IO ()
main = do
  o@Options { verbosity } <- execParser opts
  ds <- discoverDialogues o (search_paths o)

  when (null ds) $
    logWarn verbosity "No dialogues were discovered!"

  results <- mapM (evaluateDialogueIntoTest o) ds
  let summary = Autograder results

  let outpath = fromMaybe "results.json" (out_path o)
  J.encodeFile outpath summary
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Test TARGET executable against the provided stdin/out dialogues."
      <> footerDoc (
          Just ( vcat
                 [ "Each dialogue file has three parts:"
                 , nest 2 "1. A JSON object with test-specific options."
                 , nest 2 "2. A separator."
                 , nest 2 "3. Zero or more alternating input and output sections."
                 , pgraph "An input section is one or more lines of a prefix character \
                          \(by default, `>'), followed by the lines of input that will \
                          \be sent to the target executable."
                 , pgraph "An output section is one or more lines of text which will be \
                          \matched against the program's output."
                 , pgraph "For example, here is a simple dialogue file which could be used \
                          \to test an interaction with GHCi:"
                 , pgraph "For example, here is a simple dialogue file which could be used \
                          \to test an interaction with GHCi: (The `===` are not part of the file, \
                          \just used to denote the beginning and end of it."
                 , "==============================================================\n\
                   \{\n\
                   \  \"name\": \"my GHCi interaction\"\n\
                   \}\n\
                   \--------------------\n\
                   \GHCi, version 9.6.7: https://www.haskell.org/ghc/  :? for help\n\
                   \ghci> \n\
                   \> 2 + 2\n\
                   \4\n\
                   \ghci> \n\
                   \=============================================================="
                 ] ) ) )
