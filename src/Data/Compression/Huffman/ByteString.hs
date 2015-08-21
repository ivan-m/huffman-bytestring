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

import           Data.Bits
import           Data.Function   (on)
import           Data.List       (mapAccumL, sortBy)
import qualified Data.Map        as M
import           Data.Monoid
import qualified Data.PQueue.Min as P
import           Data.Tuple      (swap)
import           Data.Word       (Word, Word16, Word8)

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
blankLookup = M.fromList $ zip allBytes (repeat 0)

allBytes :: [Word8]
allBytes = [minBound .. maxBound]

-- -----------------------------------------------------------------------------

readDictionary :: LazyByteString -> M.Map Word8 Code
readDictionary = undefined

-- | Construct the raw version of the dictionary from the canonical
-- representation.
--
-- Do /not/ rely upon the ordering of the returned list.
calculateCodes :: LazyByteString -> [(Word8, Code)]
calculateCodes = assignCodes
                 . codeOrder
                 . readInDict

-- Takes advantage of the fact that sortBy is a stable sort: the
-- sorted result of @[(foo,len), (bar,len)]@ is the same.
codeOrder :: [Word16] -> [(Word8, Word16)]
codeOrder = sortBy (compare `on` snd) . zip allBytes

readInDict :: LazyByteString -> [Word16]
readInDict bs = maybe (error "Provided dictionary should have even length")
                      (const ls)
                      mb
  where
    (mb, ls) = L.foldr go (Nothing,[]) bs

    go b (Nothing, ws) = (Just (fromIntegral b `shiftL` 8), ws)
    go b (Just w, ws)  = (Nothing, (fromIntegral b .|. w) : ws)

assignCodes :: [(Word8, Word16)] -> [(Word8, Code)]
assignCodes []            = error "Should be non-empty dictionary"
assignCodes ((w0,l0):wls) = ((w0,c0):)
                            . snd
                            . mapAccumL getCode c0
                            $ wls
  where
    c0 = padCode l0 initCode

    getCode c (w,l) = (c', (w, c'))
      where
        c' = padCode l (incCode c)

-- | Representation of a code string.  This is needed rather than a
-- raw 'Word' as we need to record the length, especially for the @0@
-- code word case to know how long it is.
data Code = C { code :: !Word
              , cLen :: {-# UNPACK  #-} !Word16
              }
          deriving (Eq, Ord, Show, Read)

initCode :: Code
initCode = C { code = 0
             , cLen = 0
             }

incCode :: Code -> Code
incCode c = C cw' l'
  where
    cw' = succ (code c)
    -- Rather than fiddling with "has it gotten longer", just
    -- re-calculate it.
    l' = binaryLength cw'

padCode :: Word16 -> Code -> Code
padCode l c
  | cl >= l   = c
  | otherwise = C (shiftL (code c) (fromIntegral $ l - cl)) l
  where
    cl = cLen c

binaryLength :: Word -> Word16
binaryLength 0 = 1
binaryLength n = 1 + floor (logBase (2::Double) (fromIntegral n))
