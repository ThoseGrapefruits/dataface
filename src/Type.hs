{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Type where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text, pack)
import Data.Map (singleton)
import Control.Monad.Trans (liftIO)

import Database.Bolt (Record, Value (..), RecordValue (..), Node (..), at)

data Point = Point
  { _x :: Double
  , _y :: Double
  } deriving (Show, Eq, Ord)

data FRel = FRel
  { _fsource :: Int
  , _ftarget :: Int
  } deriving (Show, Eq)

data FGraph = FGraph
  { _fpoints :: [Point]
  , _flinks :: [FRel]
  } deriving (Show, Eq)

data Face = Face
  { _fcreator :: Text
  , _fname :: Text
  , _fgraph :: FGraph
  } deriving (Show, Eq)

data User = User
  { _username :: Text
  , _faces :: [Face]
  } deriving (Show, Eq)

instance ToJSON Value where
  toJSON (N _) = toJSON ()
  toJSON (B b) = toJSON b
  toJSON (I i) = toJSON i
  toJSON (F d) = toJSON d
  toJSON (T t) = toJSON t
  toJSON (L l) = toJSON l
  toJSON _     = undefined  -- we do not need Maps and Structures in this example

instance ToJSON FGraph where
  toJSON (FGraph p l) = object [ "points" .= p, "links" .= l ]

instance ToJSON Point where
  toJSON (Point x y) = object [ "x" .= x, "y" .= y ]

instance ToJSON FRel where
  toJSON (FRel s t) = object [ "source" .= s, "target" .= t ]

instance ToJSON Face where
  toJSON (Face n c g) = object [ "name" .= n, "creator" .= c, "graph" .= g ]

instance ToJSON User where
  toJSON (User n fs) = object [ "name" .= n, "faces" .= fs ]

toPoint :: Monad m => Value -> m Point
toPoint (L [F x, F y]) = return $ Point x y
toPoint p = fail ("Not a Point value" ++ (show p))

toPoint' :: Monad m => Node -> m Point
toPoint' (Node _ _ nodeProps) = do
  x :: Double <- (nodeProps `at` "x") >>= exact
  y :: Double <- (nodeProps `at` "y") >>= exact
  return $ Point x y

toPointsTuple :: Monad m => Node -> Node -> (m Point, [m Point])
toPointsTuple point points = (toPoint' point, traverse toPoint' points)

-- |Create face node from single record
toFaceNodes :: Monad m => Record -> m (Text, [(Point, Point)])
toFaceNodes r = do
  links :: [Record] <- (r `at` "links") >>= exact
  return $ putStrLn $ "// links"
  return $ print $ links
  -- links' :: [(m Point, m Point)] <- traverse toPointsTuple links
  name :: Text <- (r `at` "name") >>= exact
  -- return (name, links')
  return (pack "",[])
