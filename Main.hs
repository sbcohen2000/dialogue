{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Data.Aeson (FromJSON, ToJSON, (.:?), (.:?=))
import Data.Bifunctor
import Data.Char
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Options.Applicative
import Options.Applicative.Help.Pretty
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Text as T

data AutograderTest
  = AutograderTest
    { score     :: !Double
    , max_score :: !Double
    , name      :: !String
    , output    :: !String
    }
  deriving (Generic)

instance ToJSON AutograderTest

data DialogueOpts
  = DialogueOpts
    { dopts_max_score    :: !(Maybe Double)
    -- ^ The number of points that this dialogue is worth.
    , dopts_name         :: !(Maybe String)
    -- ^ The name of this test.
    , dopts_extra_args   :: !String
    -- ^ Extra command-line arguments to append to the command-line
    -- arguments provided at the command-line.
    , dopts_input_prefix :: !Text
    -- ^ The symbol which should be matched to identify that a line of
    -- the dialogue is part of the input.
    }

defaultDialogueOpts :: DialogueOpts
defaultDialogueOpts = DialogueOpts
  { dopts_max_score = Nothing
  , dopts_name = Nothing
  , dopts_extra_args = ""
  , dopts_input_prefix = ">"
  }

instance FromJSON DialogueOpts where
  parseJSON (J.Object v) = DialogueOpts
    <$> v .:?  "max_score"
    <*> v .:?  "name"
    <*> v .:?= "extra_args"
    <*> v .:?= "input_prefix"

  parseJSON invalid =
    J.prependFailure "parsing Coord failed, "
    (J.typeMismatch "Object" invalid)

  omittedField = Just defaultDialogueOpts

data Dialogue
  = Dialogue
    { opts    :: !DialogueOpts
    -- ^ Top-level options having to do with this dialogue.
    , input_sections :: ![Text]
    -- ^ The input sections, in order.
    , output_sections :: ![Text]
    -- ^ The output sections, in order.
    }

parseDialogueOpts :: Text -> Except Text DialogueOpts
parseDialogueOpts src = case J.eitherDecodeStrictText src of
  Left msg -> throwError ("Couldn't parse dialogue options:\n" <> T.pack msg)
  Right opt -> pure opt

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
  Nothing -> ([], as)

-- | Return the longest common prefix of all of the strings.
longestCommonPrefix :: [Text] -> Text
longestCommonPrefix [] = ""
longestCommonPrefix ss@(s:_)
  | all ((==p) . T.head) ss =
    T.singleton p <> longestCommonPrefix (map T.init ss)
  | otherwise = ""
  where
    p = T.head s

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
parseInputSection :: DialogueOpts -> Lines -> Except Text (Lines, Lines)
parseInputSection opts ls
  | null inpt = throwError "Expected input section to have at least one line of input."
  | otherwise = pure (removeLeadingWhitespace inpt, rest)
  where
    (inpt, rest) = spanMaybes (asInputLine opts) ls

-- | Parse an output section of the dialogue, which is identified by a
-- sequence of lines, not starting with the prefix character.
parseOutputSection :: DialogueOpts -> Lines -> Except Text (Lines, Lines)
parseOutputSection opts ls
  | null otpt = throwError "Expected output section to have at least one line of output."
  | otherwise = pure (otpt, rest)
  where
    (otpt, rest) = span (isNothing . asInputLine opts) ls

-- | Return @True@ if the line could be interpreted as a separator,
-- and @False@ otherwise.
asSeparator :: Text -> Bool
asSeparator = T.all (=='-') . T.strip

parseDialogue :: DialogueOpts -> Text -> Except Text Dialogue
parseDialogue opts = undefined

data Options
  = Options
    { target        :: !FilePath
    , extra_options :: !String
    , search_paths  :: ![FilePath]
    }

optionsParser :: Parser Options
optionsParser =
  Options
  <$> strArgument ( help "A path which points to the target executable."
                    <> metavar "TARGET" )
  <*> strOption ( long "extra-arguments"
                  <> short 'a'
                  <> help "Extra argument to pass to the target executable." )
  <*> many ( strOption ( long "path"
                         <> short 'p'
                         <> help "The path to a directory or file. \
                                 \If the path points to a directory, \
                                 \it will be searched for dialogue files. \
                                 \If it points to a file, the file will be \
                                 \interpreted as a test dialogue." ) )

pgraph :: String -> Doc
pgraph = fillSep . map pretty . words

main :: IO ()
main = do
  args <- execParser opts
  undefined
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
