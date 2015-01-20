{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Control.DeepSeq.Magic where
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.DeepSeq.Magic
-- Copyright   :  Public Domain
-- License     :  Public Domain
--
-- Maintainer  :  ezyang@cs.stanford.edu
-- Stability   :  stable
-- Portability :  portable
--
-- The functions in this module are exactly the same as
-- 'Control.DeepSeq', but without the need for an 'NFData' instance.
--
-- The semantics are not terribly well defined, since they are based on
-- what 'unpackClosure#' returns in its pointer list, but it appears
-- that we NEVER recurse into AP or PAP objects.

import GHC.Exts

-- NOTE: This is a bit slower than a normal NFData instance
-- because unpackClosure# has to allocate arrays to report
-- the results.  I tried writing a hand-rolled loop but it
-- was tricky. If this is a bottleneck I'll try again.
--
-- NOTE: Thankfully, unpackClosure# doesn't look through
-- thunks/function closures, so this behavior is actually
-- somewhat reasonable.
rnf :: a -> ()
rnf a = a `seq` rest `seq` () where
    rest = case unpackClosure# a of
            (# _, ptrs#, _ #)->
                let s# = sizeofArray# ptrs#
                    go n# | n# ==# s# = ()
                          | otherwise =
                              case indexArray# ptrs# n# of
                                (# a #) -> a `seq` go (n# +# 1#)
                in go 0#

infixr 0 $!!

deepseq :: a -> b -> b
deepseq a b = rnf a `seq` b

($!!) :: (a -> b) -> a -> b
f $!! x = x `deepseq` f x

force :: a -> a
force x = x `deepseq` x
