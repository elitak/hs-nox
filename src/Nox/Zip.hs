{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Nox.Zip (
        compress
    ) where

import Debug.Trace
import Data.Maybe
import Data.Word
import Data.BitVector as BV
import Data.Conduit
import Control.Monad.Trans.Resource
import Data.ByteString hiding (elemIndex, map, length, foldl, reverse, splitAt)
import Data.List as DL hiding (map)
import Text.Shakespeare.Text
import Data.BitString.BigEndian hiding (length, splitAt)
import qualified Data.BitString.BigEndian as BiS
import Data.Function

--decompress :: (MonadResource m) => Conduit ByteString m ByteString
compress :: (MonadResource m) => Conduit ByteString m ByteString
compress = fix (\f -> do -- XXX TODO use 'where' clause instead of 'fix'
        mbBytes <- await
        case mbBytes of
            Just bytes -> (yield $ bitString bytes) >> f
            Nothing -> return ()
    )
    =$= compressBits =$= fix (\f -> do
        mbBits <- await
        case mbBits of
            Just bits -> (trace ("final bitlen was " ++ (show $ BiS.length bits))
                         yield $ realizeBitStringStrict $ bits) >> f
            --Just bits -> do
            --    case (BiS.length bits) `divMod` 8 of
            --        (0, r) -> trace "a" leftover bits
            --        (q, 0) -> do
            --            yield $ trace "b" realizeBitStringStrict $ BiS.take (8*q) bits
            --        (q, _) -> do
            --            yield $ realizeBitStringStrict $ BiS.take (8*q) bits
            --            leftover $ trace "c" BiS.drop (8*q) bits
            --    f
            Nothing -> return ()
    )

compressBits :: (MonadResource m) => Conduit BitString m BitString
compressBits = do
    mbBits <- await
    case mbBits of
        Just bits -> do
            let vecs = encode3 bits
            yield $ fromList $ foldl (\a b -> toBits b ++ a) [] vecs
            compressBits
        Nothing -> return ()

encode3 = encode2 []
encode2 acc bits | bits == BiS.empty = acc
encode2 acc bits =
    let (first, rest) = BiS.splitAt 9 bits
        vec = fromBits . toList $ first
        lookup val = elemIndex (nat val) dict
        (byte, ninth) = (\(a,b)-> (fromBits a, fromList b)) $ splitAt 8 $ toList first
    in case (lookup vec, integerWidth . nat $ vec) of
        -- 9 bits and legitimately in table: actually consume the ninth
        -- XXX the RE'd C code doesnt seem to use any nine-bit values
        --(Just absIndex, 9) -> encode2 (toCode absIndex : acc) rest
        -- 9 bits but wasn't in dict: use first 8 bits and push back the ninth to input
        otherwise       -> encode2 ((toCode . fromJust $ lookup byte) : acc) (BiS.concat [ninth,rest])

toCode absOff = a#b where (a,b) = toPhrase absOff (reverse partitions) (length partitions - 1)
toPhrase absOff ((bits, lBnd):rest) pNdx | absOff < lBnd = toPhrase absOff rest (pNdx-1)
                                         | otherwise     = (bitVec 4 pNdx, bitVec bits (absOff - lBnd))

-- TODO: make sure c-encrypt output is the same as this impl. run against many input files and compare. discrepancies prly due to c-encrypt not using 9-bit values in the dictionary...
-- TODO: make sure those files actually load into the game
-- TODO: wirte decompress. test on my files then on official (should break at lz77 refs)
-- TODO: use ollydbg to figuer out the rest? bpaccess that 89th byte of So_Beach?
-- TODO: in so_beach.map, streams diverge at first 8x3 repeated pattern (source byte 0x58) length-distance should be 0x38, 8, then 0x8 0x10 to repeat it 2 more times. however:
-- (entire sequence is 40 bits)
-- 08 03 81 b0 04 (shift this left 1 bit)
-- 10 07 03 60 08
-- first 7 bits decoded to 0x104 in table
-- 03 81 b0 04 00 (33 bits)

-- TODO these tables can probably be reorganized into Enum types that facilitate lookups somehow
partitions = [ (0x02, 0x00)
             , (0x03, 0x04)
             , (0x03, 0x0C)
             , (0x04, 0x14)
             , (0x04, 0x24)
             , (0x04, 0x34)
             , (0x04, 0x44)
             , (0x04, 0x54)
             , (0x04, 0x64)
             , (0x04, 0x74)
             , (0x04, 0x84)
             , (0x04, 0x94)
             , (0x04, 0xA4)
             , (0x05, 0xB4)
             , (0x05, 0xD4)
             , (0x05, 0xF4)
             ]

-- 9 bit words divided into partitions
dict = [   0x100, 0x101 
         , 0x102, 0x103

         , 0x104, 0x105
         , 0x106, 0x107
         , 0x108, 0x109
         , 0x10A, 0x10B

         , 0x10C, 0x10D
         , 0x10E, 0x10F
         , 0x000, 0x020
         , 0x030, 0x0FF

         , 0x001, 0x002
         , 0x003, 0x004
         , 0x005, 0x006
         , 0x007, 0x008
         , 0x009, 0x00A
         , 0x00B, 0x00C
         , 0x00D, 0x00E
         , 0x00F, 0x010

         , 0x011, 0x012
         , 0x013, 0x014
         , 0x015, 0x016
         , 0x017, 0x018
         , 0x019, 0x01A
         , 0x01B, 0x01C
         , 0x01D, 0x01E
         , 0x01F, 0x021

         , 0x022, 0x023
         , 0x024, 0x025
         , 0x026, 0x027
         , 0x028, 0x029
         , 0x02A, 0x02B
         , 0x02C, 0x02D
         , 0x02E, 0x02F
         , 0x031, 0x032

         , 0x033, 0x034
         , 0x035, 0x036
         , 0x037, 0x038
         , 0x039, 0x03A
         , 0x03B, 0x03C
         , 0x03D, 0x03E
         , 0x03F, 0x040
         , 0x041, 0x042

         , 0x043, 0x044
         , 0x045, 0x046
         , 0x047, 0x048
         , 0x049, 0x04A
         , 0x04B, 0x04C
         , 0x04D, 0x04E
         , 0x04F, 0x050
         , 0x051, 0x052

         , 0x053, 0x054
         , 0x055, 0x056
         , 0x057, 0x058
         , 0x059, 0x05A
         , 0x05B, 0x05C
         , 0x05D, 0x05E
         , 0x05F, 0x060
         , 0x061, 0x062

         , 0x063, 0x064
         , 0x065, 0x066
         , 0x067, 0x068
         , 0x069, 0x06A
         , 0x06B, 0x06C
         , 0x06D, 0x06E
         , 0x06F, 0x070
         , 0x071, 0x072

         , 0x073, 0x074
         , 0x075, 0x076
         , 0x077, 0x078
         , 0x079, 0x07A
         , 0x07B, 0x07C
         , 0x07D, 0x07E
         , 0x07F, 0x080
         , 0x081, 0x082

         , 0x083, 0x084
         , 0x085, 0x086
         , 0x087, 0x088
         , 0x089, 0x08A
         , 0x08B, 0x08C
         , 0x08D, 0x08E
         , 0x08F, 0x090
         , 0x091, 0x092

         , 0x093, 0x094
         , 0x095, 0x096
         , 0x097, 0x098
         , 0x099, 0x09A
         , 0x09B, 0x09C
         , 0x09D, 0x09E
         , 0x09F, 0x0A0
         , 0x0A1, 0x0A2
         , 0x0A3, 0x0A4
         , 0x0A5, 0x0A6
         , 0x0A7, 0x0A8
         , 0x0A9, 0x0AA
         , 0x0AB, 0x0AC
         , 0x0AD, 0x0AE
         , 0x0AF, 0x0B0
         , 0x0B1, 0x0B2

         , 0x0B3, 0x0B4
         , 0x0B5, 0x0B6
         , 0x0B7, 0x0B8
         , 0x0B9, 0x0BA
         , 0x0BB, 0x0BC
         , 0x0BD, 0x0BE
         , 0x0BF, 0x0C0
         , 0x0C1, 0x0C2
         , 0x0C3, 0x0C4
         , 0x0C5, 0x0C6
         , 0x0C7, 0x0C8
         , 0x0C9, 0x0CA
         , 0x0CB, 0x0CC
         , 0x0CD, 0x0CE
         , 0x0CF, 0x0D0
         , 0x0D1, 0x0D2

         , 0x0D3, 0x0D4
         , 0x0D5, 0x0D6
         , 0x0D7, 0x0D8
         , 0x0D9, 0x0DA
         , 0x0DB, 0x0DC
         , 0x0DD, 0x0DE
         , 0x0DF, 0x0E0
         , 0x0E1, 0x0E2
         , 0x0E3, 0x0E4
         , 0x0E5, 0x0E6
         , 0x0E7, 0x0E8
         , 0x0E9, 0x0EA
         , 0x0EB, 0x0EC
         , 0x0ED, 0x0EE
         , 0x0EF, 0x0F0
         , 0x0F1, 0x0F2

         , 0x0F3, 0x0F4
         , 0x0F5, 0x0F6
         , 0x0F7, 0x0F8
         , 0x0F9, 0x0FA
         , 0x0FB, 0x0FC
         , 0x0FD, 0x0FE
         , 0x110, 0x111
         -- That last partition was only up to index 0x0d...
         -- Are indexes 0x0e (or maybe 0x10) and up treated specially?
         ]
