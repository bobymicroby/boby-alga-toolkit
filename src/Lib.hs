module Lib
  ( User (..),
    Request (..),
    handle,
    followersOf,
    following,
    suggest,
    followersGraph,
    followingGraph,
    suggestGraph,
    generate,
    Graph (..),
  )
where

import Algebra.Graph as G
import Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.AdjacencyMap.Algorithm as AL
import Algebra.Graph.ToGraph as TG
import System.Random.MWC (GenIO, asGenIO, withSystemRandom)
import RandomGraphs as RG
import Control.Monad (join)
import Data.Bifunctor (bimap)

newtype User = User {phone :: String} deriving (Eq, Ord, Show)

data Request
  = Add User
  | Remove User
  | Follow User User
  | Unfollow User User

handle :: Request -> Graph User -> Graph User
handle (Add a) g = Overlay (Vertex a) g
handle (Remove a) g = G.removeVertex a g
handle (Unfollow a b) g = G.removeEdge a b g
handle (Follow a b) g = Overlay (Connect (Vertex a) (Vertex b)) g

-- returns all user that follow our target
followersOf :: User -> Graph User -> [User]
followersOf target =
  map fst . filter (\(_, v) -> target == v) . G.edgeList

-- returns all users that our target follows
following :: User -> Graph User -> [User]
following target =
  map snd . filter (\(v, _) -> target == v) . G.edgeList

-- returns the first n 'friends of friends' aka 'second level friends'
suggest :: User -> Int -> Graph User -> [User]
suggest user n graph =
  let adjacencyMap = TG.toAdjacencyMap graph
      levels = AL.bfs [user] adjacencyMap
   in take n $ concat $ drop 2 levels

-- returns a graph with our target in the center with connections to the people it follows
followingGraph :: User -> Graph User -> Graph User
followingGraph target g = Connect (Vertex target) (G.vertices $ following target g)

-- returns a graph with our target in the center with connections from the people that follow it
followersGraph :: User -> Graph User -> Graph User
followersGraph u g = Connect (Vertex u) (G.vertices $ followersOf u g)

-- returns n `friends of friends` for the target
suggestGraph :: User -> Int -> Graph User -> Graph User
suggestGraph u i g = Connect (Vertex u) (G.vertices $ suggest u i g)


generate ::
  -- | n, The number of nodes
  Int ->
  -- | The probability for any pair of nodes to be connected
  Double ->
  -- | The resulting graph (IO required for randomness)
  IO (Graph User)
generate n p = do
  gen <- withSystemRandom . asGenIO $ return
  graphInfo <- RG.erdosRenyiGraph gen n  p
  let 
    e = RG.edges graphInfo
    ef = filter (\x-> fst x /= snd x ) e
    v = fmap (\x ->  (  User (  "rando-"++show(fst x)) , User (  "rando-"++show(snd  x)))  ) ef
    g = G.edges v
  return g