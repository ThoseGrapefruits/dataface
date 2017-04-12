{-# LANGUAGE OverloadedStrings #-}

module Data (ServerState (..), WebM (..), constructState, querySearch, queryMovie, queryGraph,
             queryUser, createUser, queryFaceGraph) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Crypto.BCrypt
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Map.Strict (fromList, (!))
import Data.Monoid ((<>))
import Data.Pool (Pool, createPool)
import Data.Text (Text, toLower)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.Typeable (typeOf)
import Database.Bolt

import Type

-- |A pool of connections to Neo4j server
data ServerState = ServerState { pool :: Pool Pipe }

-- |Reader monad over IO to store connection pool
type WebM = ReaderT ServerState IO

-- |Search movie by title pattern
querySearch :: Text -> BoltActionT IO [Movie]
querySearch q = do records <- queryP cypher params
                   nodes <- traverse (`at` "movie") records
                   traverse toMovie nodes
  where cypher = "MATCH (movie:Movie) WHERE movie.title =~ {title} RETURN movie"
        params = fromList [("title", T $ "(?i).*" <> q <> ".*")]

-- |Returns movie by title
queryMovie :: Text -> BoltActionT IO MovieInfo
queryMovie title = do result <- head <$> queryP cypher params
                      T title <- result `at` "title"
                      L members <- result `at` "cast"
                      cast <- traverse toCast members
                      return $ MovieInfo title cast
  where cypher = "MATCH (movie:Movie {title:{title}}) " <>
                 "OPTIONAL MATCH (movie)<-[r]-(person:Person) " <>
                 "RETURN movie.title as title," <>
                 "collect([person.name, " <>
                 "         head(split(lower(type(r)), '_')), r.roles]) as cast " <>
                 "LIMIT 1"
        params = fromList [("title", T title)]

-- |Returns movies with all it's actors
queryGraph :: Int -> BoltActionT IO MGraph
queryGraph limit = do records <- queryP cypher params
                      liftIO . putStrLn $ "// records: "
                      liftIO . print $ records
                      nodeTuples <- traverse toMovieNodes records
                      liftIO . putStrLn $ "// nodeTuples: "
                      liftIO . print $ nodeTuples
                      let movies = fst <$> nodeTuples
                      liftIO . putStrLn $ "// movies: "
                      liftIO . print $ movies
                      let actors = nub $ concatMap snd nodeTuples
                      liftIO . putStrLn $ "// actors: "
                      liftIO . print $ actors
                      let actorIdx = fromJust . (`lookup` zip actors [0..])
                      let modifyTpl (m, as) = (m, actorIdx <$> as)
                      let indexMap = fromList $ modifyTpl <$> nodeTuples
                      liftIO . putStrLn $ "// indexMap: "
                      liftIO . print $ indexMap
                      let mkTuples (m, t) = (`MRel` t) <$> indexMap ! m
                      let relations = concatMap mkTuples $ zip movies [length actors..]
                      liftIO . putStrLn $ "// relations: "
                      liftIO . print $ relations
                      return $ MGraph (actors <> movies) relations
  where cypher = "MATCH (m:Movie)<-[:ACTED_IN]-(a:Person) " <>
                 "RETURN m.title as movie, collect(a) as cast " <>
                 "LIMIT {limit}"
        params = fromList [("limit", I limit)]

buildFGraph :: [Record] -> IO FGraph
buildFGraph records = do nodeTuples <- traverse toFaceNodes records
                         liftIO . putStrLn $ ""
                         liftIO . putStrLn $ "// nodeTuples: "
                         liftIO . print $ nodeTuples
                         let faces = sel1 <$> nodeTuples
                         liftIO . putStrLn $ ""
                         liftIO . putStrLn $ "// faces: "
                         liftIO . print $ faces
                         let points = nub $ (map sel2 nodeTuples)
                         liftIO . putStrLn $ ""
                         liftIO . putStrLn $ "// points: "
                         liftIO . print $ points
                         let connectedPoints = nub $ concatMap sel3 nodeTuples
                         liftIO . putStrLn $ ""
                         liftIO . putStrLn $ "// connectedPoints: "
                         liftIO . print $ connectedPoints
                         let faceIdx = fromJust . (`lookup` zip points [0..])
                         let modifyTpl (name, point, linked) = (point, faceIdx <$> linked)
                         let indexMap = fromList $ (map modifyTpl nodeTuples)
                         liftIO . putStrLn $ ""
                         liftIO . putStrLn $ "// indexMap: "
                         liftIO . print $ indexMap
                         let mkTuples (m, t) = (`FRel` t) <$> indexMap ! m
                         let relations = concatMap mkTuples $ zip points [0..]
                         liftIO . putStrLn $ ""
                         liftIO . putStrLn $ "// relations: "
                         liftIO . print $ relations
                         return (FGraph points relations)

-- |Returns face with all it's points
queryFaceGraph :: Int -> BoltActionT IO FGraph
queryFaceGraph limit = do records <- queryP cypher params
                          liftIO . print . typeOf $ records
                          liftIO . putStrLn $ ""
                          liftIO . putStrLn $ "// records: "
                          liftIO . print $ records
                          graph <- liftIO $ (buildFGraph records)
                          liftIO . print . typeOf $ graph
                          liftIO . putStrLn $ ""
                          liftIO . putStrLn $ "// graph: "
                          liftIO . print $ graph
                          return $ graph
  where cypher = "MATCH (f0:Face)-[:STARTS_AT]->(p0:Point)<-[:LINK*0..10]-(end:Point)-[l:LINK]-(start:Point) " <>
                 "RETURN f0.name as name, {point: start, linked: COLLECT(DISTINCT end)} as group"
        params = fromList [("limit", I limit)]

queryUser :: Text -> BoltActionT IO User
queryUser username = do records <- queryP cypher params
                        liftIO . putStrLn $ "// RECORDS FOR USER " ++ (show username)
                        liftIO . print $ records
                        graph <- liftIO $ (buildFGraph records)
                        return (User username [(Face username "face" graph)])
  where cypher = "MATCH (u:User {username: {username} })-[:CREATED]->(f0:Face)-[:STARTS_AT]->(p0:Point)<-[:LINK*0..10]-(end:Point)-[l:LINK]-(start:Point) " <>
                 "RETURN u.username as username, f0.name as name, {point: start , linked: COLLECT(DISTINCT end)} as group"
        params = fromList [("username", T username)]

-- |Create user with the given username and password
createUser :: Text -> Text -> BoltActionT IO User
createUser username password = do
    records <- queryP cypher params
    liftIO . putStrLn $ "// CREATING USER" ++ (show username)
    liftIO . print $ records
    return (User username [])
  where cypher = "CREATE (u:User {username: {username}, passwordHash: {passwordHash}})" <>
                 "RETURN u"
        params = fromList [("username", T (toLower username)), ("passwordHash", T password)]

-- |Returns faces by username of owner
-- queryFace :: Text -> BoltActionT IO Face
-- queryFace username = do result <- head <$> queryP cypher params
                        -- T id <- result `at` "id"
                        -- T name <- result `at` "name"
                        -- return $ Face id name startsAt
  -- where cypher = "MATCH (u:User {username:{username}})-[:CREATED]->(f0:Face)<-[:LINK*]-(ps:Point) " <>
                 -- "OPTIONAL MATCH (f0:Face)-[:CHILD*{isFork: false}]->(fx:Face) " <>
                 -- "RETURN coalesce(fx, f0) as face, collect(ps) as points"
        -- params = fromList [("username", T username)]


-- |Create pool of connections
constructState :: BoltCfg -> IO ServerState
constructState bcfg = do let stripes = 4
                         let timeout = 500 -- ms
                         let resources = 1 -- resources per stripe
                         pool <- createPool (connect bcfg) close stripes timeout resources
                         return (ServerState pool)
