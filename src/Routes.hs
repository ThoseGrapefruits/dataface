{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Crypto.BCrypt
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Pool (withResource)
import Data.Text.Lazy (Text, pack, unpack, toStrict, fromStrict)
import Database.Bolt

import Web.Scotty.Trans (ActionT, file, param, json, rescue)
import Web.Scotty.Cookie

import Type
import Data

-- |Run BOLT action in scotty 'ActionT' monad transformer
runQ :: BoltActionT IO a -> ActionT Text WebM a
runQ act = do ss <- lift ask
              liftIO $ withResource (pool ss) (`run` act)

-- |Main page route
mainR :: ActionT Text WebM ()
mainR = file "index.html"

-- |Stylesheet route
styleR :: ActionT Text WebM ()
styleR = file "style.css"

-- |Graph response route
graphR :: ActionT Text WebM ()
graphR = do limit <- param "limit" `rescue` const (return 5)
            graph <- runQ $ queryGraph limit
            json graph

-- |Face graph response route
faceGraphR :: ActionT Text WebM ()
faceGraphR = do limit <- param "limit" `rescue` const (return 5)
                -- |graph <- runQ $ queryFaceGraph limit
                graph <- runQ $ queryLatestFaces limit
                liftIO . print $ "//// GRAPH: "
                liftIO . print $ graph
                json graph

-- |User response route
getUserR :: ActionT Text WebM ()
getUserR = do username <- param "username"
              user <- runQ $ queryUser username
              liftIO . putStrLn $ "//// GET USER: "
              liftIO . print $ user
              json user

getHashedPassword :: Text -> IO C8.ByteString
getHashedPassword password = do
  Just hashedPassword <- (hashPasswordUsingPolicy slowerBcryptHashingPolicy (C8.pack (unpack password)))
  return hashedPassword

byteStringToText :: IO C8.ByteString -> IO Text
byteStringToText byteString = do
  bs <- byteString
  return (pack (C8.unpack bs))

-- |User creation route
createUserR :: ActionT Text WebM ()
createUserR = do username <- param "username"
                 password <- param "password"
                 hashedPassword <- liftIO $ byteStringToText $ getHashedPassword $ password
                 user <- runQ $ (createUser username (toStrict hashedPassword))
                 setSimpleCookie "username" $ username
                 setSimpleCookie "password" $ (toStrict hashedPassword)
                 liftIO . putStrLn $ "//// createUserR: "
                 liftIO . print $ user
                 json user

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
