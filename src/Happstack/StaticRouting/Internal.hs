{-# LANGUAGE FunctionalDependencies, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
    FlexibleContexts, DeriveFunctor, PatternGuards, TupleSections, AllowAmbiguousTypes, TypeApplications #-}
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
import Data.List(intercalate,find)
import Data.Maybe

-- | Static routing tables consisting of handlers of type 'a'.
data Route a =
    Dir Segment (Route a)
  | Param (Route a)
  | Handler EndSegment CheckApply a
  | Choice [Route a]
  deriving Functor

data Segment =
    StringS String | ParamS
  deriving (Show, Eq, Ord)

type EndSegment = (Maybe Int, Method)

type CheckApply = [String] -> Bool

-- | Support for varying number of arguments to 'path' handlers.
class Path m hm h r | h -> m r where
  pathHandler  :: forall r'. (m r -> hm r') -> h -> hm r'
  arity        :: h -> Int
  canBeApplied :: h -> [String] -> Bool

instance (
    FromReqURI v
  , ServerMonad hm
  , Path m hm h r
  ) => Path m hm (v -> h) r where
    pathHandler trans f = applyPath (pathHandler trans . f)
    arity f = 1 + arity @m @hm (f undefined)
    canBeApplied f [] = False
    canBeApplied f (s:ss) = case (fromReqURI s) of
                                Just p -> canBeApplied @m @hm (f p) ss
                                Nothing -> False


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
  arity _ = 0
  canBeApplied _ _ = True

-- | Pop a path element if it matches the given string.
dir :: String -> Route a -> Route a
dir = Dir . StringS

-- | Pop a path element, and store it to use with handler
param :: Route a -> Route a
param = Param

-- | Combine several route alternatives into one.
choice :: [Route a] -> Route a
choice = Choice

-- | Expect the given method, and exactly 'n' more segments, where 'n' is the arity of the handler.
path :: forall m hm h r r'. Path m hm h r
     => Method -> (m r -> hm r') -> h -> Route (hm r')
path m trans h = Handler (Just (arity @m @hm h), m) (canBeApplied @m @hm h) (pathHandler trans h)

-- | Expect zero or more segments.
remainingPath :: Method -> h -> Route h
remainingPath m = Handler (Nothing,m) (\_ -> True)

newtype RouteTree a =
  R { unR :: Trie.TrieMap Map Segment (Map EndSegment a) } deriving (Show, Functor)

type Segments = ([Segment],EndSegment)

-- | Compile a route into a 'RouteTree'.  Turn overlapping routes into 'Nothing'
routeTreeWithOverlaps :: Route a -> RouteTree (Maybe (CheckApply,a))
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
routeTree :: RouteTree (Maybe (CheckApply,a)) -> Either String (RouteTree (CheckApply,a))
routeTree t | not $ null os =
                Left $ unlines $
                  "Happstack.StaticRouting: Overlapping handlers in" :
                  map (("  "++) . showSegments) os
            | not $ null is =
                Left $ unlines $
                  "Happstack.StaticRouting: Unreachable handler due to ignored parameter in" :
                  map (("  "++) . showSegments) is
            | otherwise = Right $ fmap fromJust t

  where os = [ (ss, es) | (ss, m) <- Trie.toList (unR t)
             , (es, Nothing) <- Map.toList m
             ]
        is = [ (ss, es) | (ss, m) <- Trie.toList (unR t)
             , (es@(Just p, _), _) <- Map.toList m
             , p < length (filter ((==) ParamS) $ ss)
             ]
showSegments :: Segments -> String
showSegments (ss, es) = concatMap showSegment ss ++ showEndSegment es
  where

  showSegment :: Segment -> String
  showSegment (StringS e) = "dir " ++ show e ++ " $ "
  showSegment (ParamS) = "param (used in handler) $ "

  showEndSegment :: EndSegment -> String
  showEndSegment (Just a, m) = "<handler> -- with method " ++ show m ++ " and arity " ++ show a
  showEndSegment (Nothing, m) = "remainingPath $ <handler> -- with method " ++ show m

flatten :: Route a -> [(Segments, (CheckApply, a))]
flatten = f where
  f (Dir s r) = map (first (first (s:))) (f r)
  f (Param r) = map (first (first (ParamS:))) (f r)
  f (Handler e ca a) = [(([], e), (ca, a))]
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
            RouteTree (CheckApply,(m Response)) -> m (Maybe Response)
dispatch t = do
  rq  <- askRq
  case dispatch' [] (rqMethod rq) (rqPaths rq) t of
    Just (rq', h) -> Just `liftM` localRq (\newRq -> newRq{ rqPaths = rq'}) h
    Nothing       -> return Nothing

-- | Dispatch a request given a method and path.  Give priority to more specific paths.
-- 'params' holds path segments that where matched 'ParamS' segment.
dispatch' :: forall a . [String] -> Method -> [String] -> RouteTree (CheckApply,a) -> Maybe ([String], a)
dispatch' params m ps (R t) = dChildren ps `mplus` fmap (params ++ ps,) dNode
  where
  -- most specific: look up a segment in the children and recurse
  dChildren :: [String] -> Maybe ([String], a)
  dChildren (p:ps') = ((Map.lookup (StringS p) (Trie.children1 t)) >>= dispatch' params m ps' . R)
              `mplus` ((Map.lookup (ParamS) (Trie.children1 t)) >>= dispatch' (params ++ [p]) m ps' . R)
  dChildren []      = Nothing
  dNode :: Maybe a
  dNode = do
    -- Find a handler that does not need any more segments
    em <- Trie.lookup [] t
    -- Select one that matches number of parameters or one that will ignore them (created with 'remainingPath')
    (ac,h)  <- (Map.lookup (Just (length ps + length params), m) em) `mplus` (Map.lookup (Nothing, m) em)
    -- Make sure that params can converted with fromReqURI
    if (ac (params ++ ps))
     then return h
     else Nothing

