{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Options.Applicative (Parser, option, metavar, long, auto, help)
import qualified Options.Applicative as Opts
import           System.Hardware.Serialport
import           System.IO
import           Text.Read (readMaybe)
import           Web.Scotty


data CLIArgs = CLIArgs
  { listenPort :: Int
  } deriving (Eq, Ord, Show)


cliArgsParser :: Parser CLIArgs
cliArgsParser = CLIArgs
  <$> (option auto (long "port" <> metavar "PORT" <> help "port to listen on"))


-- | Parses the command line arguments for this program.
parseArgs :: IO CLIArgs
parseArgs = Opts.execParser $
  Opts.info
    (Opts.helper <*> cliArgsParser)
    (Opts.fullDesc <> Opts.progDesc "Reads temperature from an Arduino")


main :: IO ()
main = do
  CLIArgs{ listenPort } <- parseArgs

  let port = "/dev/ttyUSB0"
  h <- hOpenSerial port defaultSerialSettings
    { timeout = 1000
    , commSpeed = CS115200
    }

  temperatureRef <- newIORef (0 :: Double)

  let serialReaderThread :: IO ()
      serialReaderThread = forever $ do
        line <- hGetLine h
        case readMaybe $ T.unpack $ T.strip $ T.pack line of
          Just d -> writeIORef temperatureRef d
          Nothing -> putStrLn $ "ingoring serial input " ++ line

  race_ serialReaderThread $ do

    scotty listenPort $
      get "/temperature" $ do
        temp <- liftIO $ readIORef temperatureRef
        html $ mconcat [TL.pack (show temp)]
