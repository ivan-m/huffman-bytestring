{-# LANGUAGE BangPatterns, CPP #-}

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

import           Control.Arrow   (second, (***))
import           Data.Bits
import           Data.Bool       (bool)
import           Data.Function   (on)
import           Data.List       (foldl', mapAccumL, partition, sortBy)
import qualified Data.Map        as M
import qualified Data.PQueue.Min as P
import           Data.Tuple      (swap)
import           Data.Word       (Word8)

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
import Data.Word   (Word)
#endif

-- -----------------------------------------------------------------------------

-- | Encode a tag using a static Huffman encoding tree.
huffmanEncode :: LazyByteString -> LazyByteString
huffmanEncode = error "still to be defined"

type LazyByteString = L.ByteString

type EncDict = ()

fromDictionary :: LazyByteString -> Huffman
fromDictionary d = H { encoder = ()
                     , decoder = dictionaryDecoder d
                     }

data Huffman = H { encoder :: !EncDict
                 , decoder :: !DTree
                 }
               deriving (Eq)

prettyHuffman :: Huffman -> String
prettyHuffman (H e d) = unlines [ "Encoder:"
                                ,  show e
                                ,  "Decoder:"
                                ,  show d
                                ]

printHuffman :: Huffman -> IO ()
printHuffman = putStr . prettyHuffman

-- -----------------------------------------------------------------------------

-- | Create the representation of a canonical Huffman encoding for all
-- possible bytes using the sample data.
createDictionary :: LazyByteString -> LazyByteString
createDictionary = B.toLazyByteString
                   . mconcat
                   . map B.word8
                   . canonicalLengths
                   . buildTree
                   . sortFreq
                   . freqCount

data HuffmanTree = Node Int HuffmanTree HuffmanTree
                 | Leaf Int Word8
                 deriving (Eq, Show, Read)

-- | Ordering solely by the frequency of the tree.
instance Ord HuffmanTree where
  compare = compare `on` treeFreq

treeFreq :: HuffmanTree -> Int
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

buildTree :: [(Int, Word8)] -> HuffmanTree
buildTree = go . P.fromAscList . map (uncurry Leaf)
  where
    go p = case P.splitAt 2 p of
             ([]  ,_)      -> error "Encoding: Empty input!"
             ([t] ,_)      -> t
             ([t1 ,t2],p') -> let t = Node (treeFreq t1 + treeFreq t2) t1 t2
                              in go (P.insert t p')
             _             -> error "Encoding: shouldn't happen."

type CodeLen = Word8

-- | Length of code in Huffman tree of each possible byte in order
-- (i.e. the result should have a length of 256).
--
-- It suffices to use Word8 for each length as the longest code will
-- be of length @|alphabet| - 1@ (maximum depth of binary a tree with
-- @n@ nodes is @n-1@).  As the size of the alphabet is 256, the
-- longest code will be of length 255 and hence will fit in a single
-- Word8.
canonicalLengths :: HuffmanTree -> [CodeLen]
canonicalLengths = M.elems . go 0 blankLookup
  -- We use a 'Map' here as we want to be sure we return the list in
  -- order of the corresponding original input byte, so this way we
  -- get the sorting.
  where
    go !d m (Leaf _ w)     = M.insert w d m
    go !d m (Node _ t1 t2) = let d1 = d + 1
                             in d1 `seq` go d1 (go d1 m t1) t2

blankLookup :: (Num a) => M.Map Word8 a
blankLookup = M.fromList $ zip allBytes (repeat 0)

allBytes :: [Word8]
allBytes = [minBound .. maxBound]

numBits :: Int
numBits = finiteBitSize (minBound :: Word8)

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
codeOrder :: [CodeLen] -> [(Word8, CodeLen)]
codeOrder = sortBy (compare `on` snd) . zip allBytes

readInDict :: LazyByteString -> [CodeLen]
readInDict = L.unpack

assignCodes :: [(Word8, CodeLen)] -> [(Word8, Code)]
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
              , cLen :: {-# UNPACK  #-} !Int
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

padCode :: CodeLen -> Code -> Code
padCode l c
  | cl >= l'   = c
  | otherwise = C (shiftL (code c) (l' - cl)) l'
  where
    l' = fromIntegral l
    cl = cLen c

binaryLength :: Word -> Int
binaryLength 0 = 1
binaryLength n = 1 + floor (logBase (2::Double) (fromIntegral n))

-- -----------------------------------------------------------------------------

dictionaryDecoder :: LazyByteString -> DTree
dictionaryDecoder = codesToTree . calculateCodes

data DTree = Branch DTree DTree
           | Value Word8
           deriving (Eq, Ord, Show, Read)

codesToTree :: [(Word8,Code)] -> DTree
codesToTree []      = error "Cannot create empty tree"
codesToTree [(w,_)] = Value w
codesToTree wcs     = uncurry Branch
                      . (toT *** toT)
                      . partition (snd . snd)
                      . map getMSB
                      $ wcs
  where
    toT = codesToTree . map (second fst)

    getMSB = second splitMSB

    splitMSB c@(C cw l) = (c {cLen = pred l}, testBit cw l)

-- TODO: work out how to deal with trailing 0s on the end so we don't
-- spuriously try to convert those to valid values.
decode :: Huffman -> LazyByteString -> LazyByteString
decode (H { decoder = dt }) = B.toLazyByteString
                              . snd
                              . L.foldl' decodeByte (dt, mempty)
  where
    decodeByte tb byte = foldl' decodeBit tb (map (testBit byte) [0..numBits - 1])

    decodeBit (Branch lt rt, bld) b = case bool lt rt b of
                                        Value w -> (dt, bld `mappend` B.word8 w)
                                        t'      -> (t', bld)
    decodeBit (Value w, _bld)    _b = error $ "Reached Value of " ++ show w ++ " too early!"
