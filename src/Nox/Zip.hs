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
import Data.ByteString as BS hiding (elemIndex, map, foldl1)
import Data.List as DL hiding (map)
import Text.Shakespeare.Text
import Data.BitString.BigEndian as BiS

--decompress :: (MonadResource m) => Conduit ByteString m ByteString
compress :: (MonadResource m) => Conduit ByteString m ByteString
compress = do
    maybeBytes <- await
    case maybeBytes of
        Just bytes -> do
            --yield $ pack $ map (fromIntegral . nat) $ group_ 8 $ foldl1 (\a b -> a#b) $ map encode (unpack bytes)
            let (first, rest) = BiS.splitAt 9 $ bitString bytes
            let bvec = fromBits $ toList first
            let (byte, ninth) = encode bvec
            trace ("split " ++ showHex ( fromBits $ DL.take 9 (toBits bvec))) $ 
                trace ("retured " ++ show byte ++"("++showHex byte++") and " ++ show ninth) $
                    yield $ (realizeBitStringStrict . fromList . toBits) byte
            case size ninth of
                0 -> leftover $ realizeBitStringStrict rest
                1 -> leftover $ realizeBitStringStrict $ BiS.concat [fromList $ toBits ninth, rest]
            compress
        Nothing -> return ()

encode :: BV -> (BV, BV)
--encode byte = trace ([st|encoded #{byte} as #{a},#{b}|]) a#b
-- vec can be 8 on 9 bits long, or even shorter
encode vec = --trace ("called with " ++ show vec ++ " aka " ++ (showHex  vec))$
             case (elemIndex (nat vec) dict, size vec < 9, integerWidth $ nat vec) of
                  (Just index, False, 9) -> (toWord index, bitVec 0 0) -- 9 bits and legitimately in table: actually consume the ninth
                  (Just index, True,  x) -> --trace ("width " ++ show x)
                                            (toWord index, bitVec 0 0)
                  otherwise              -> (fst $ encode byte, ninth) -- was 9-bits-and-low or wasnt in dict: try again without the trailing bit
             where (byte, ninth) = (\(a,b)->(fromBits a, fromBits b)) $ DL.splitAt 8 (toBits vec)

toWord absOff = a#b where (a,b) = toPhrase absOff partitions 0 0
toPhrase absOff ((bits, uBnd):[]  ) lBnd pNdx                 = (bitVec 4  pNdx   , bitVec bits (absOff - uBnd))
toPhrase absOff ((bits, uBnd):rest) lBnd pNdx | absOff < uBnd = (bitVec 4 (pNdx-1), bitVec bits (absOff - lBnd))
                                              | otherwise     = toPhrase absOff rest uBnd (pNdx+1)


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
         ]
