{-# LANGUAGE BangPatterns #-}

{- |
   Module      : Data.Compression.Huffman.ByteString
   Description : Canonical Huffman encoding for ByteStrings.
   Copyright   : Copyright (c) 2015 Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan Miljenovic <Ivan.Miljenovic@iag.com.au>

 -}
module Data.Compression.Huffman.ByteString where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as L

import           Data.Function   (on)
import           Data.List       (sortBy)
import qualified Data.Map        as M
import           Data.Monoid
import qualified Data.PQueue.Min as P
import           Data.Tuple      (swap)
import           Data.Word       (Word16, Word8)

-- -----------------------------------------------------------------------------

-- | Encode a tag using a static Huffman encoding tree.
huffmanEncode :: LazyByteString -> LazyByteString
huffmanEncode = error "still to be defined"

type LazyByteString = L.ByteString

-- -----------------------------------------------------------------------------

-- | Create the representation of a canonical Huffman encoding for all
-- possible bytes using the sample data.
createDictionary :: LazyByteString -> LazyByteString
createDictionary = B.toLazyByteString
                   . mconcat
                   . map B.word16LE
                   . canonicalLengths
                   . buildTree
                   . sortFreq
                   . freqCount

data Huffman = Node Int Huffman Huffman
             | Leaf Int Word8
             deriving (Eq, Show, Read)

-- | Ordering solely by the frequency of the tree.
instance Ord Huffman where
  compare = compare `on` treeFreq

treeFreq :: Huffman -> Int
treeFreq (Node f _ _) = f
treeFreq (Leaf f _)   = f

freqCount :: LazyByteString -> M.Map Word8 Int
freqCount = L.foldl' go blankLookup
  where
    go m a = M.adjust succ a m

-- Returns keys in increasing frequency order.
sortFreq :: M.Map Word8 Int -> [(Int, Word8)]
sortFreq = map swap
           . sortBy (compare `on` snd)
           . M.toAscList

buildTree :: [(Int, Word8)] -> Huffman
buildTree = go . P.fromAscList . map (uncurry Leaf)
  where
    go p = case P.splitAt 2 p of
             ([]  ,_)      -> error "Encoding: Empty input!"
             ([t] ,_)      -> t
             ([t1 ,t2],p') -> let t = Node (treeFreq t1 + treeFreq t2) t1 t2
                              in go (P.insert t p')
             _             -> error "Encoding: shouldn't happen."

-- | Length of code in Huffman tree of each possible byte in order
-- (i.e. the result should have a length of 256).
--
-- Need to use 'Word16' here as in a pathological case, the length of
-- the code of the most unfrequent byte will be 256 (and hence
-- overflow a single Word8 value).
canonicalLengths :: Huffman -> [Word16]
canonicalLengths = M.elems . go 0 blankLookup
  where
    go !d m (Leaf _ w)     = M.insert w d m
    go !d m (Node _ t1 t2) = let d1 = d + 1
                             in d1 `seq` go d1 (go d1 m t1) t2

blankLookup :: (Num a) => M.Map Word8 a
blankLookup = M.fromList $ zip [minBound .. maxBound] (repeat 0)

-- -----------------------------------------------------------------------------

type Code = Int

readDictionary :: LazyByteString -> M.Map Word8 Code
readDictionary = undefined

-- readInDict :: LazyByteString ->
