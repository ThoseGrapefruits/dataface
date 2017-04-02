{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Control.Monad.Except (MonadIO, MonadError, catchError, liftIO)
import Control.Applicative ((<$>))
import System.Environment (getEnv)

import Database.Bolt

import SimpleServer

-- |Default configuration for localhost neo4j server
defaultConfig :: BoltCfg
defaultConfig = def {user = "dataface", password = "dataface"}

-- |Run with stack exec dataface-exe (with PORT environment variable set)
main :: IO ()
main = run `catchError` failMsg
    where run = do port <- return 8080
                   config <- readConfig `catchError` const (return defaultConfig)
                   runServer port config
          readConfig = do
              bolt <- getEnv "GRAPHENEDB_BOLT_URL"
              user <- read <$> getEnv "GRAPHENEDB_BOLT_USER"
              pass <- read <$> getEnv "GRAPHENEDB_BOLT_PASSWORD"
              let (host, port) = let sp = last (elemIndices ':' bolt)
                                 in (read $ take sp bolt, read $ drop (sp+1) bolt)
              return def { user = user, password = pass, host = host, port = port }

failMsg :: (MonadError e m, MonadIO m, Show e) => e -> m ()
failMsg e = liftIO $ putStrLn ("Oops: " ++ show e)
