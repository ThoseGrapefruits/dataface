{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Pool (withResource)
import Data.Text.Lazy (Text, toStrict)
import Database.Bolt
import Web.Scotty.Trans (ActionT, file, param, json, rescue)

import Type
import Data

-- |Run BOLT action in scotty 'ActionT' monad transformer
runQ :: BoltActionT IO a -> ActionT Text WebM a
runQ act = do ss <- lift ask
              liftIO $ withResource (pool ss) (`run` act)

-- |Main page route
mainR :: ActionT Text WebM ()
mainR = file "index.html"

-- |Face response route
faceR :: ActionT Text WebM ()
faceR = do username <- param "username" `rescue` const (return 100)
           graph <- runQ $ queryFaces username
           json graph

-- |Point response route
pointR :: ActionT Text WebM ()
faceR = do username <- param "username" `rescue` const (return 100)
           graph <- runQ $ queryPoints faceId
           json graph

-- |Graph response route
graphR :: ActionT Text WebM ()
graphR = do limit <- param "limit" `rescue` const (return 100)
            graph <- runQ $ queryGraph limit
            json graph

-- |Search response route
searchR :: ActionT Text WebM ()
searchR = do q <- param "q" :: ActionT Text WebM Text
             results <- runQ $ querySearch (toStrict q)
             json results

-- |Movie response route
movieR :: ActionT Text WebM ()
movieR = do t <- param "title" :: ActionT Text WebM Text
            movieInfo <- runQ $ queryMovie (toStrict t)
            json movieInfo
