{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant.Client (ServantError)
import System.Environment (lookupEnv)
import qualified Data.Text as T
import Data.Either (either)
import Twfy.Client

import Options.Applicative

data Options = Options {
  optApiKey :: ApiKey
  , optCommand :: CommandOptions
  } deriving (Show)

data CommandOptions = ConstituenciesOpts
                      {
                        constituenciesOptsDate :: Maybe T.Text
                      , constituenciesOptsSearch :: Maybe T.Text
                      }
                    | ConstituencyOpts
                      {
                        constituencyOptsName :: Maybe T.Text
                      , constituencyOptsPostCode :: Maybe T.Text
                      }
                    | MpOpts
                      {
                        mpOptsId :: Maybe Int
                      , mpOptsConstituencyName :: Maybe T.Text
                      , mpOptsConstituencyPostCode :: Maybe T.Text
                      } deriving (Show)

parseText :: ReadM T.Text
parseText = eitherReader $ (\s -> Right $ T.pack s)

constituenciesOpts :: Parser CommandOptions
constituenciesOpts = ConstituenciesOpts
  <$> (optional $ option parseText
       (long "date"
        <> short 'd'
        <> metavar "DATE"
        <> help "Constituencies as of date"))
  <*> (optional $ option parseText
       (long "search"
        <> short 's'
        <> metavar "SEARCH"
        <> help "Constituency search name"))

constituencyNameOption :: Parser (Maybe T.Text)
constituencyNameOption = optional $ option parseText
  (long "name"
   <> short 'n'
   <> metavar "NAME"
   <> help "Constituency name")

constituencyPostCodeOption :: Parser (Maybe T.Text)
constituencyPostCodeOption = optional $ option parseText
  (long "postcode"
   <> short 'p'
   <> metavar "POSTCODE"
   <> help "Constituency post code")

constituencyOpts :: Parser CommandOptions
constituencyOpts = ConstituencyOpts
  <$> constituencyNameOption
  <*> constituencyPostCodeOption

mpIdOption :: Parser (Maybe Int)
mpIdOption = optional $ option auto
  (long "id"
   <> short 'i'
   <> metavar "ID"
   <> help "MP's person ID")

mpConstituencyNameOption :: Parser (Maybe T.Text)
mpConstituencyNameOption = optional $ option parseText
  (long "constituency"
   <> short 'c'
   <> metavar "CONSTITUENCY"
   <> help "MP's constituency name")

mpConstituencyPostCodeOption :: Parser (Maybe T.Text)
mpConstituencyPostCodeOption = optional $ option parseText
  (long "postcode"
   <> short 'p'
   <> metavar "POSTCODE"
   <> help "MP's constituency post code")

mpConstituencyOpts :: Parser CommandOptions
mpConstituencyOpts = MpOpts
  <$> mpIdOption
  <*> mpConstituencyNameOption
  <*> mpConstituencyPostCodeOption

commandParser :: Parser CommandOptions
commandParser = subparser (
  command "constituencies" (info constituenciesOpts
                            (progDesc "Get constituency info" ))
  <>
  command "constituency" (info constituencyOpts
                          (progDesc "Get constituency info" ))
  <>
  command "mp" (info mpConstituencyOpts
                (progDesc "Get MP info")))

apiKeyOpt :: Parser ApiKey
apiKeyOpt = option parseText
  (long "key"
   <> short 'k'
   <> metavar "APIKEY"
   <> help "API Key")

optionsParser :: Parser Options
optionsParser = Options <$> apiKeyOpt <*> commandParser

appParser :: ParserInfo Options
appParser = info (helper <*> optionsParser)
      (fullDesc
       <> progDesc "TWFY CLI"
       <> header "twfy-api-client")

readApiKey :: IO (Maybe ApiKey)
readApiKey = do
  envKey <- lookupEnv "TWFY_API_KEY"
  let textKey = fmap T.pack envKey
  return textKey

displayResult :: Show a => (Either ServantError a) -> IO ()
displayResult = either
  (\e -> putStrLn $ "Error: " ++ show e)
  (\x -> print x)

runCommand :: Client -> CommandOptions -> IO ()
runCommand x (ConstituenciesOpts a b) = getConstituencies x >>= displayResult -- TODO: add params to actual API calls
runCommand x (ConstituencyOpts a b) = getConstituency x a b >>= displayResult
runCommand x (MpOpts a b c) = getMP x a b c (Just False) >>= displayResult

main :: IO ()
main = do
  -- TODO: fix how we decide whether to use env or command line value
  envApiKey <- readApiKey
  opts <- execParser appParser
  putStrLn $ show opts
  let apiKey = maybe (optApiKey opts) id envApiKey
  c <- client apiKey Nothing Nothing
  runCommand c (optCommand opts)
