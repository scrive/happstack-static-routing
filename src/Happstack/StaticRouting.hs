{-# OPTIONS_HADDOCK prune #-}

-- | Support for static routing tables in Happstack.  The routing
-- tables are order independent as long as:
--
-- * if any two handlers overlap, one of them handles a more specific
-- path than the other.  The more specific handler is then tried
-- first.
--
-- Routing tables are constructed from 'dir', 'path', 'remainingPath',
-- 'choice', and 'param'.
--
-- A routing table is compiled by using 'compile'.  The result is an
-- overlap report, and a prefix tree that is used to efficiently
-- dispatch requests by means of 'dispatch'.
--
-- See the file @src\/Happstack\/StaticRouting\/Test.hs@ in the distribution
-- for examples.

module Happstack.StaticRouting
  ( Route
  , compile
  , choice
  , dir
  , param
  , path
  , Path
  , remainingPath
  ) where

import Happstack.StaticRouting.Internal
