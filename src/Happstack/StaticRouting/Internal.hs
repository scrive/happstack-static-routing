{-# LANGUAGE OverlappingInstances, FunctionalDependencies, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
    FlexibleContexts, DeriveFunctor, PatternGuards, TupleSections #-}
{-# OPTIONS_HADDOCK hide #-}

module Happstack.StaticRouting.Internal where

import Debug.Trace

import Happstack.Server(askRq, rqPaths, rqMethod, localRq, ServerMonad, Method,
  HasRqData, methodM, look, FromReqURI, fromReqURI, notFound, Response, toResponse, FilterMonad)
import Control.Monad(msum, MonadPlus, mzero, mplus, liftM)
import Control.Monad.IO.Class(MonadIO)
import Control.Arrow(first, second)
import qualified Data.ListTrie.Map as Trie
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(intercalate)
import Data.Maybe

-- | Static routing tables consisting of handlers of type 'a'.
data Route a =
    Dir Segment (Route a)
  | Handler EndSegment a
  | Choice [Route a]
  deriving (Show, Functor)

newtype Segment =
    StringS String
  deriving (Show, Eq, Ord)

type EndSegment = (Maybe Int, Method)

-- | Support for varying number of arguments to 'path' handlers.
class Path m hm h r | h r -> m where
  pathHandler :: forall r'. (m r -> hm r') -> h -> hm r'
  arity       :: hm r -> h -> Int

instance (
    FromReqURI v
  , ServerMonad hm
  , Path m hm h r
  ) => Path m hm (v -> h) r where
    pathHandler trans f = applyPath (pathHandler trans . f)
    arity m f = 1 + arity m (f undefined)

-- | Pop a path element and parse it using the 'fromReqURI' in the
-- 'FromReqURI' class.  Variant of Happstack.Server.path without 'mzero'
applyPath :: (FromReqURI a, ServerMonad m) => (a -> m b) -> m b
applyPath handle = do
    rq <- askRq
    case rqPaths rq of
        (p:xs) | Just a <- fromReqURI p
                            -> localRq (\newRq -> newRq{rqPaths = xs}) (handle a)
        _ -> error "Happstack.StaticRouting.applyPath"

instance Path m hm (m r) r where
  pathHandler trans mr = trans mr
  arity _ _ = 0

-- | Pop a path element if it matches the given string.
dir :: String -> Route a -> Route a
dir = Dir . StringS

-- | Combine several route alternatives into one.
choice :: [Route a] -> Route a
choice = Choice

-- | Expect the given method, and exactly 'n' more segments, where 'n' is the arity of the handler.
path :: forall m hm h r r'. Path m hm h r
     => Method -> (m r -> hm r') -> h -> Route (hm r')
path m trans h = Handler (Just (arity (undefined::hm r) h), m) (pathHandler trans h)

-- | Expect zero or more segments.
remainingPath :: Method -> h -> Route h
remainingPath m = Handler (Nothing,m)

newtype RouteTree a =
  R { unR :: Trie.TrieMap Map Segment (Map EndSegment a) } deriving (Show, Functor)

type Segments = ([Segment],EndSegment)

-- | Compile a route into a 'RouteTree'.  Turn overlapping routes into 'Nothing'
routeTreeWithOverlaps :: Route a -> RouteTree (Maybe a)
routeTreeWithOverlaps r =
  R $ foldr (\((ps,es),m) ->
              Trie.insertWith (Map.unionWith merge)
                  ps
                  (Map.singleton es (Just m)))
      Trie.empty
      (flatten r)
  where merge (Just _) _ = Nothing -- overlap
        merge Nothing  m = m

-- | Check for overlaps in a 'RouteTree', returning either an error
-- message in case of an overlap, or a 'RouteTree' without overlaps.
routeTree :: RouteTree (Maybe a) -> Either String (RouteTree a)
routeTree t | null os   = Right $ fmap fromJust t
            | otherwise =
                Left $ unlines $
                  "Happstack.StaticRouting: Overlapping handlers in" :
                  map (("  "++) . showSegments) os

  where os = [ (ss, es) | (ss, m) <- Trie.toList (unR t)
             , (es, Nothing) <- Map.toList m
             ]

showSegments :: Segments -> String
showSegments (ss, es) = concatMap showSegment ss ++ showEndSegment es
  where

  showSegment :: Segment -> String
  showSegment (StringS e) = "dir " ++ show e ++ " $ "

  showEndSegment :: EndSegment -> String
  showEndSegment (Just a, m) = "<handler> -- with method " ++ show m ++ " and arity " ++ show a
  showEndSegment (Nothing, m) = "remainingPath $ <handler> -- with method " ++ show m

flatten :: Route a -> [(Segments, a)]
flatten = f where
  f (Dir s r) = map (first (first (s:))) (f r)
  f (Handler e a) = [(([], e), a)]
  f (Choice rs) = concatMap f rs

-- | Compile routes or return overlap report.  Returns 'Left e' in
-- case of order-dependent overlap between handlers, where 'e'
-- describes the overlap.  Returns 'Right h', where h is a compiled
-- handler that returns 'Nothing' in case no matching handler was
-- found, otherwise 'Just response'.
compile :: (MonadIO m, HasRqData m, ServerMonad m, FilterMonad Response m) =>
           Route (m Response) -> Either String (m (Maybe Response))
compile r = case t of
              Left s -> Left s
              Right t -> Right $ dispatch t
  where t = routeTree $ routeTreeWithOverlaps r

-- | Dispatch a request given a route.  Give priority to more specific paths.
dispatch :: forall m . (MonadIO m, HasRqData m, ServerMonad m, FilterMonad Response m) =>
            RouteTree (m Response) -> m (Maybe Response)
dispatch t = do
  rq  <- askRq
  case dispatch' (rqMethod rq) (rqPaths rq) t of
    Just (rq', h) -> Just `liftM` localRq (\newRq -> newRq{ rqPaths = rq'}) h
    Nothing       -> return Nothing

-- | Dispatch a request given a method and path.  Give priority to more specific paths.
dispatch' :: forall a . Method -> [String] -> RouteTree a -> Maybe ([String], a)
dispatch' m ps (R t) = dChildren ps `mplus` fmap (ps,) dNode
  where
  -- most specific: look up a segment in the children and recurse
  dChildren :: [String] -> Maybe ([String], a)
  dChildren (p:ps') = Map.lookup (StringS p) (Trie.children1 t) >>= dispatch' m ps' . R
  dChildren []      = Nothing
  dNode :: Maybe a
  dNode = Trie.lookup [] t >>= \em ->
   -- or else a 'path' taking a given specific number of remaining segments
           Map.lookup (Just (length ps), m) em
  -- least specific: a 'remainingPath' taking any number of remaining segments
     `mplus` Map.lookup (Nothing, m) em

