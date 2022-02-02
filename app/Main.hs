module Main where

import           Data.Char           (isDigit)
import           Data.List           (partition)
import           Data.Map            ((!))
import qualified Data.Maybe          as M
import qualified Data.Vector         as V
import           Options.Applicative
import           Problems.Problems
import           Text.Printf

newtype AOCOpts = AOCOpts {_specRaw :: String}

data AOCSpec = AOCSpec
  { _day  :: Int,
    _part :: Maybe Char
  }

instance Show AOCSpec where
  show (AOCSpec day part) =
    case part of
      Just part' -> show day ++ [part']
      Nothing    -> show day

parseOpts :: Parser AOCOpts
parseOpts =
  AOCOpts
    <$> strOption
      ( long "spec"
          <> short 's'
          <> metavar "SPEC"
          <> help "Day to run - in \\d?\\d[ab] format"
      )

parseSpec :: String -> AOCSpec
parseSpec s = AOCSpec day part
  where
    -- TODO use regex?
    (day', part') = partition isDigit s
    day = read day' :: Int
    part = M.listToMaybe part'

execSpec :: AOCSpec -> IO ()
execSpec spec@(AOCSpec day part)
  | M.isNothing part = do
    showResult spec {_part = Just 'a'} (fst pair)
    showResult spec {_part = Just 'b'} (snd pair)
  | part == Just 'a' = showResult spec (fst pair)
  | part == Just 'b' = showResult spec (snd pair)
  | otherwise = error "unexpected part"
  where
    pair = dayToProblemFns ! day
    showResult spec f = f >>= \f -> putStrLn $ show spec ++ ": " ++ show f

main :: IO ()
main = do
  execSpec . parseSpec . _specRaw =<< execParser opts
  where
    opts = info (parseOpts <**> helper) fullDesc
