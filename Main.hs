module Main where

import Options.Applicative

data Options
  = Options
    {
    }

optionsParser :: Parser Options
optionsParser = pure Options

main :: IO ()
main = putStrLn "Hello, Haskell!"
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Test TARGET executable against the provided stdin/out dialogues." )
