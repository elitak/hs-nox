{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Nox.Crypt (
        FileType(..)
      , decrypt
      , encrypt
    ) where

import Prelude hiding (concat)
import Data.ByteString.Base64 (decodeLenient)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toChunks)
import Data.Conduit
import Data.ByteString hiding (map, pack)
import Control.Monad.Trans.Resource
import Data.Word
import Data.Bits
import Data.Binary.Strict.Get
import Data.Binary.Put
import Text.Heredoc

data FileType = Unused0
              | Player
              | Map
              | Thing
              | Modifier
              | GameData
              | Monster
              | SoundSet
              deriving (Eq, Enum)

data CryptMode = Decrypt | Encrypt deriving (Eq, Enum)

decrypt :: (MonadResource m) => FileType -> Conduit ByteString m ByteString
decrypt = crypt Decrypt
encrypt :: (MonadResource m) => FileType -> Conduit ByteString m ByteString
encrypt = crypt Encrypt

crypt :: (MonadResource m) => CryptMode -> FileType -> Conduit ByteString m ByteString
crypt mode fileType = toWords =$= cryptWord mode fileType =$= fromWords

-- FIXME leftovers used wrong! `leftover` passes to next conduit, not self!

toWords :: (MonadResource m) => Conduit ByteString m Word32
toWords = do
    maybeBytes <- await
    case maybeBytes of
        (Just bytes) -> do
            let (Right w1, bytes' ) = runGet getWord32be bytes
            let (Right w2, bytes'') = runGet getWord32be bytes'
            yield w1
            yield w2
            leftover bytes''
        Nothing -> return ()

fromWords :: (MonadResource m) => Conduit Word32 m ByteString
fromWords = do
    word1 <- await
    word2 <- await
    case (word1, word2) of
        (Just w1, Just w2) -> do
            yield $ concat $ map (concat . toChunks . runPut . putWord32be) [w1, w2]
        (Just w1, Nothing) -> leftover w1 -- XXX Maybe pad with 0's here, if that's valid?
        (Nothing, Nothing) -> return ()

cryptWord :: (MonadResource m) => CryptMode -> FileType -> Conduit Word32 m Word32
cryptWord mode fileType = do
    let table = cryptTable fileType
    let offset = case mode of Encrypt -> 0x400
                              Decrypt -> 0x418
    word1 <- await
    word2 <- await
    case (word1, word2) of
        (Just w1, Just w2) -> do
            let cycles = 8
            let (w1', w2') = mangle table offset w1 w2 cycles
            yield (w2' `xor` table !! (offset + 2*cycles+1))
            yield (w1' `xor` table !! (offset + 2*cycles  ))
        (Just w1, Nothing) -> leftover w1 -- XXX Maybe pad with 0's here, if that's valid?
        (Nothing, Nothing) -> return ()

mangle :: [Word32] -> Int -> Word32 -> Word32 -> Int -> (Word32, Word32)
mangle table offset w1 w2 0     = (w1, w2)
mangle table offset w1 w2 times =
    let w1'      = w1 `xor` table !! offset
        sum1     = useTable table w1'
        w2'      = w2 `xor` (sum1 `xor` table !! (offset+1))
        sum2     = useTable table w2'
    in
        mangle table (offset+2) (w1' `xor` sum2) w2' (times-1)

useTable :: [Word32] -> Word32 -> Word32
useTable table word = table !! fromIntegral (0x000 + word `shiftR` 24 .&. 0xff)
                    + table !! fromIntegral (0x100 + word `shiftR` 16 .&. 0xff)
                `xor` table !! fromIntegral (0x200 + word `shiftR`  8 .&. 0xff)
                    + table !! fromIntegral (0x300 + word `shiftR`  0 .&. 0xff)

-- XXX TODO try benchmarking the access time of these data (using "criterion" pkg),
-- then switching to some form of Array.
-- FIXME: nothing is generated at compile-time, yet. Use cabal build --ghc-option=-ddump-splices --verbose
-- to check. Maybe check C/LLVM files for good measure, too.

-- TODO: i'd like best for something like this to work:
--cryptTable GameData = $([|readFile "gamedata.blob"])
-- that way i can just have the binary table in a file that gets included at compile-time.
cryptTable fileType = $([|
    let asWord32s = do
            empty <- isEmpty
            if empty
                then return []
            else do
                v <- getWord32be
                rest <- asWord32s
                return (v : rest)
        (Right x, _) = runGet asWord32s $ (decodeLenient . pack) $ case fileType of
            GameData -> [str|CgOQHzAM0OuRk1NO8XUkGsueDFqKZWMzU8/RAmXNIxku50sH8MjzPPfCnaiuR4YvW6tRPpiuTOVn
                            |Ss/TOppVjVQyNG/bWZB1Zr5FneN8UpBkn2tFVC9RfotY7MOKB1GHT6Nzgd7EsJKyLGZkpICDN/lS
                            |n4hdjCpVsT6qNmmLqVd3nHi+3uiLKBUCuUrVT1srvXYI+dpkbVeXmw2nPhMKRp+P4u6mx+wicNlO
                            |EqufA4+i4PEUUeFWpX+thV7tCzsvXrjiKPqZjJhnKOCerwnrN4GGExOX+RZb4hBPq0ZpPTmkTIYd
                            |K9u2hhRijgs2kU5yZCeJQbqQ7NnbfQaqEWzvoNXQnn3xaptwKossAkVNDh2nTlTECcSHrzUFFqk/
                            |vSBKmaf2kFF8ufyWMzH6HYKg9lGDlPyrQgzE+eOQq46NmqxmT5ZdHoU0Veh5H52L0btx2PO6rqVz
                            |mqNwxMiah4A8BREwgatt7r8wD4Vj/GJt4JbVl/qi2bu+EGrE6WgDv5zunCAmNNcNa+NISEy15ePc
                            |3LIjgShS9KLHczUCmaaqzwSBhvo9+hryvORUfxQmgKAqWPZa/tJigr9ZD4W2dFSykiG191Vay6HG
                            |uqmaT7+o7YEavO/3/aUFoeKaLlIBHe8fwJajJCTsw3YZBmxW7Cf/OZ9tGIz1acwrVmczKHiPoGYp
                            |xZKcZhYrHIk+akwXHvTVMOqA+X6I8LCxV3eqkXBPytJEzW/zfGe52RO45rtne1jIvCq3mLXur3dx
                            |VBZrG7najZ/SRV2nY3BCHmS+xlKyDFacJlu7RbMUZjJIgUAUcYe8lV+kodbqknEuxFKhJaWMYWN9
                            |dBuEEZpUmgaE1OeXfak1KMoYomzbLAUPMHK8b8/9B1j0pr6TrRdjm2shQ+5DgSYJ1WJJhRClekWp
                            |jOv8l4zAfVP4ziZoTmf2iv3g3ZY3no/gMv+KCLiiTv4Z7JBgMLdRI4g9JbxqmP3gSLruWP/GERYU
                            |Xyd++80DMmp3h4228+/AOPPAcv3mhOkhlTlUxlPsCQdGYchIj/Iu3e8EgFkSbUgMgoLlcEOC/TdY
                            |mOkvyNIzTiLG65o80VTozZMUmYF5zsX2f0wa2QuEMSYjPpJ4+SkhmtD6PigaMKfKa4rQ9fhUl9aL
                            |6mIhJ0UvzYBE4+LviBUnhrd+iQpWrVkzIUMwBsCyTFQNgcV2HIfPTzR0o8ERHx9au9NjVPBCfdX2
                            |shaHOV9zXlq5WdVN5AkAAY5Wcusm3eSFcIcW4nU4ojqzXGVMSrsvEnbrC6WvBH8XfFxDvyzXBJA2
                            |mTEoVug+FvKLWqoW/ol7ZYqUFUSQdXDv0F04J/GDvDojWZ+BR6UtDVE8CbJQhfHuzCbUXIcZcXhA
                            |Lp8C0jgM3ZKlXGmWRQD7cTCILKEK1JjqYEuBm4pvUBPGe9V62N5kGjW69v7ZsLV6YLGFeJcX5lv8
                            |lhUvJru8s5JjaWwG/29Gr8aR6gOTi6w3890QzUj0Vo+pSOCTrpweoJHQ2R/tM+TZDvBgZpRCh3rN
                            |hkX9GQf7TSBm4kE7v4z9k8/+ukojfsLuX/G+dA29K7eo4WmFurqMmlSAH1CLvZcP+LnvvCoIIPXD
                            |PHHRusS+DL8jItBgP1cWF6143UprINXDSxUJPeBB9+8DYG+Ks7/ouTubDKdJiEPeyZYhPDXLouZc
                            |8caqOtP19/TYoVZFauYbZ9LHgvYh9hJnng8VbMJm3mLa9ZqFpT0nVmSVSREsKvJb8FDW9FnsXVDo
                            |jawmvYBv3FfrY9tMVXE7AtseEnfLh0WnOkxyFtUh1BKTqCBAn8XEwuzTG9M5sfrzBMi+LkLzXUjL
                            |qiPnHyjLwgySrngc/uulqwIMIuxh8LK07DEae598irJ+zTQGbujDt3vjtWvSykDeKN6CRlJO5iX9
                            |6YkTkrQtro78+Ro8nW+2Ybss/iTWLraKBT9zn3jpz3m6qCL3Os+ippgOwV7626auRDHuIgncfHtz
                            |kG5ARySU5tX/2n+xBRmcq3hmybQCMZsBJ9vP8nUfA7A/znHpN3RIT8yEPm1sCqxo8tB6AYIheVfV
                            |LPVis5kbg6PP6OXBBxG+G0NVwg9bD5wcHUE/NEsQvLUwjdBzsCEk178aUOU1F7HqtTGpR33bEwWP
                            |TTR5pHJYbDqgcZVSVangPRewSdepLd43Wwt0IZ0g1I7YFJavJOWhmvLQaB6AOYIW02wbEixq2EoA
                            |K3K4r6oXxb/NBMQVKPK20V8Gaxdl7SStwragob4NIC90/4JZIzOSDcHIPjvGC77nC9efaN4Sn9HD
                            |1fNVZU44K2h2oCl0ad0/j425Q+315l6pw7Z8TBmaA3J/3ZkuQaFziGpJd6sXIC0ueBMgjgpje5Su
                            |iQA9hoxBNXr69QuVMoLYER2D3JX4R9cFO/ygDpE7jzHfuzSQHHUiRNZyJsCjxOnHKNBtm9u5ZTk0
                            |FBQRQ2p10b9DLaJ2G0dswCiOtEE/r0TlF3U+rbix9UD9/wEhWualt4FRZvj8YL0Ff9dw4IV8Szi5
                            |2Xb41yGcLEV4uyO8nSbjO95qtBXvHuiVksUynixWRZVtpfhhw5NRECER5hnB924KqVW5L5hkkCVh
                            |PCtFW1p6pn3qI8mTRnJDb1kiDDq8B5Y831bwmP/b7IfN8pBASzjJywfNJG8xYdD7dmXHsC1YOFW2
                            |eH6ncFjIUP3Ucvn1iRZMUUIQjfXvHbR0DGxe+LIU3YU+xMoAhy4GAK7pUQ0XJKTj6rPu0X1wXRJm
                            |+k8AUJeOePMG7p1svi1T3qbns0pB5uyqWHpHjyeY07nGoq9qXz1p5mHAWrHGLQ9JTJ9cuPFWK5Lz
                            |BSaVVD0jJuMly7bjHI2i/F+P54aAzvjRo9UC4U2SffAs0jIrS2JZH3todfkExgHcq6Er7oivTQg3
                            |GVQv3t460sMSgeynejhQIVlZSHKNnYzwTBTGSXDqezxs7ednIqY98W921VdS4eaL8QIpagFy0OZ5
                            |4ynPuYE+C4kyZJwoOWZJ/lDD/H4WvL7eqW+8ArJ/kpF1oCnR7ouT0/ZiHCTsKODqetI98+1zdJoe
                            |mOa/CDIJqr6e/kd2xFQQ77GBYP+TwHmhvMQuYW08ue6jWLGQsjhvkBAPPZNKfyM1wqKeDoCrFTKu
                            |beDFYczCLmYk7X5MG3j1eiJybWZhYGF0WdiSlPKitiht/eTwVNt2klVvDb7kHvAiOjB1ArrTNh6n
                            |InjUEP3QLB/8yZj6z1UI7oieY6UeaOBPjXZ1YaEh6WD5Qxo9nyGJJU/vHYP8ot8JDKoh0QToxVAF
                            |VdYy/LcRIdsq24O58hnEddmT6IbTFBzpRnLRYUfPSBsWeQJAwRHCnbeaDdKmnvxpxsSb20NHc1Y2
                            |DbRHkNcrhjnkvf0td2EeaD/rRDYoIvFl8RxTXTd0XXVNemIYO81B9RMWp/CFGw9TJU2090tWQ08G
                            |X2iTq+iQZgsD1N/M865KIl5+NtSmQOIFuKkZrhVfYz2xlg6VPTT7f7F9EisO6L0TY6VQVsnV8rgY
                            |IsD7skRx/RbSEfAy/XFe9GGUrxeuem0fgcH356K8iOTqOMj6QrjuCLb99crcWMEtM+O3+Y5glNLS
                            |ou3mKHdCxKqgHo6fOuUaZD5jHwDKm4k4zzaC+wbC34rliFHqT1RQrruzS1VwmahZfDbwP2LSe8y1
                            |iOiIgLp6VULu1/wTTXDKkqKdxfQgETz9ZiJ8pWr1dEi2eAWM2HObLhM6tkJe3vHz7XbWQwRSWmie
                            |II3ZwrEOCXe/cfUD994eo2G30J895jhAESPcetfavNzqwJhDad5HWj6ID4Ido/LumN3qkP58Vqfh
                            |SY59V2WNhTzm5krag/Z/TaWngyIpY3sSIB6Fa0OOLI3oVWixknvn/frZgLk725wsdg6LN1PYkcdb
                            |5Ph9rONfA/Gf+VgNm76PjtXW/I544q4r/xDMuMmAvuHeqiiPsUgGg02sYiaH9YY/M1Ia7tJ890/8
                            |p8ZF/wOjiO9Lwn1OwhQHZ6IsZifcrYsMTBEUIX1pYqYPIytaq5/dtZdyM018Mxip5T4QLk8IxTQ7
                            |sTMf6Wudr95sNiPcomjEuj3B/agqIYihv62PHeww/A1cVgWqNATIdwwmj5XiUjkhJcjy6PsiUl14
                            |GqM5M/LLAphtxvDMPZ5HusuA3osviFu9njCzGh3Y274rUTgdmgaM+m/+ycc0XiABlsDm7ul3E2/L
                            |q7rHk0uBnN/Zn/uOfhTAIhJP0il5vRBPPDtkcDaf4LuGOpxNiFFu0GUkMU3SVV2SB86Gvd8DksUY
                            |j3I36ogbyCe/4rhGqIpWQIEFm9fQw7Fgf6YzEt+/16/GVpWS1iO7uSIdh5VELJpQIgsxpcqGHK79
                            |jltKPg6t1IrZOZpBftGlX0PklKBfI+5T3j2Lua/k4yBCaQKsMCuyyLsM5b7Hp21qUORwmCBB/3ZL
                            |MKFT59kvsD8zZg8oQp06Du/BwkzQwujmUQs2ktLrGsPyQ6WfIzs9zfJo7AaVNgN7V02tTPTx+/OW
                            |WkCqE2QYCGoGTir6y+/IWr5yWi9INxeQ9FoVA8lodMxvJJKmp/q6ZGgjqJ5GZyNrUqfyXvWnFRuH
                            |GISvrN3obTIBGVmrM/DUZxx5j6NXZzjHjWpCjvyfsbLt4Gakt0FG+HHKOfuxByOxVrta8AIw3Hm+
                            |t0f9MgtSK2YgijKAAkdcTGWOc8mRUtveOMnBIm2bBJrVwO8hV1qmijsxUrajZpFet8Crs0/ZO6gq
                            |QMpptW0wzjnHp3ETrMkE+kdKL7crrKRRwRRNHYRYGyQE8N3c7f1R0FFsVs9HuDWgvZWjubCHHo+A
                            |WX7RNgYfSrCdplEchz6Byge6KnQHYgGmtHOm/nEZeGn2w7tW3z6sZv48MHFre9/piW64wxGYHqtq
                            |h0QBenqoLYfxXdlRaCuswivK/MmRad28xR+ik1oYEKlqtTfZhtbonfDHN9/SeeUiz+KT79FpDmw4
                            |71NL7FcEcE54TqGjzxF8XUCVmirjwWAWswu4pVUzVyG/a00+jd+pyYYIa2ItP0QWAR3Lvxo0J57r
                            |GWHjQplWPyFhSOPSts4LgO87tgGpWrihHeHYsrW4CSb4QpE3xjMuZApjEStA6ZQqNJvy1SXpJhr2
                            |Zzne949HrD9o34HuL+MXMjeTpz8qxN6EvXdch9erjsOuXm5XpGK6yQK6tb5FnKZ3PmwaFpjCSV9C
                            |UuZW45feur6w8V/aGTXgjhgE2jws3cIlXWzAGzg1PNh1/94/ABH8COJpeymBz8P3eTm+IcMWn96+
                            |g0qz+bxOAZfjf5qax6TofcHapFV9bRKBEZyH/AAP4Wy5wdljuzsat0cP0W+FLmRmhcVkwIqNHI0K
                            |HAmWrGHEr0xf4GQx47VtuRhHElcRSzK0vtsAcSiv63HMi4v7w63K4NWK2qoC6XFjwm+FbIkK8nNU
                            |JjnMn2oUlVD8C5LQewdHPukpdEsv6ocCwvdjJBPNStOZnasQ5TLWSXoknQj/rKf+wYDvovFYXtHI
                            |CdECQVjVL10ws/5NqTD9dv2tOzn15bxFpzYM/PFZh413euzxof2/SvmautPeVweXthxQVyPoYs5e
                            |qcfKVOXrrwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFTl669eqcfKI+hizrYcUFfeVweX+Zq606H9
                            |v0p3euzx8VmHjac2DPz15bxF/a07Oakw/XYws/5NWNUvXQnRAkFYXtHIgO+i8QAAAAEAAAAA
                            |]
            Modifier -> [str|hh2+dA0h33UopErf9z3FQmUH7mnTAp4d9EEwIm/rq49165ZLRuIGP6PPfQAlzMFQ+hKjQtj2Irra
                            |M3fgK0ZPIbN+QF3Atr2Ha2NUf6dOf0q8AAI9PQN0aBTUaruzGv1REwc/oEwk0A7tNaV55luWkeoI
                            |JWYUiYo429qDJ4mvDByWOYoennv7bB9nlTBQPf9qeca1thGK+1v1hbZem4HhDM9QvIrR/jLnnzjK
                            |ufErQQNFlrdECQ94W/jhyJErdAWOBPTUa0aqXKXHS2TkWr9sakoKpMH+qXiKw5LKElblGQm64uIk
                            |HEE1U08pElgFgrpvMTwR6tg/g+gzQJOA8LI3UdhODysS3hiyjzI7tq2V71p8K6BJ+YKfQzEIK5JJ
                            |AYoItYIPWUZn1mriwwa6jwsNSJeDfRQO/cd3I1h578e+1KNus2V/zM662S0NY0PMidyFBCwTRSA9
                            |22lY6GjSwxJrSNkbrOW+sgNzjI0aJ4GunaDOS6HG3vcWj+/alWgow8059v5xLpXALJj8aEHwlE9y
                            |zuRw9/OoYQsDqWFBN+6HFeIdOT5dKd2PtaTdszYrJ0itu6WIqElowxawn2L5tg/2n+Lb2xk5aKsM
                            |jRsiPRiB9nPzCdcxg26vtImhTPcL0qdQXPvyCdEAECmOzgD5EhxecXtwiEvysbLR8TGB5umL54E5
                            |kHG9NpHSP0AH59EbIweD6Acsl21YFarKo6SQ0CqUrCH2cLogZmgRCycZqsZhNsfLyhgLWcC4vJn+
                            |BYid/uwVxFnbYd56WNnIPooQ3MeJckozW/uUqYZxX1NtMSDYM7aaASWRYNTgNctflfpQUKCbkBUF
                            |ucDy7OWamRz1lmrMXtV6NTAVRkRRimjXmKqXv1y7qGQiBR2k2cOXluiPNuV7oJ2wmX5LSP/DQbzs
                            |ZfG5BQBpAS/zK3ed/+i+7y8NUqqOhQYm5fe+7F9iqCecpumOrA0D5jAohis3GF4Pdt/PiLQBndNr
                            |fqNSj43rGy4oAoDIucCP8myPxsvNriMLBXa63b6cMriVimnaSj/wGdB0eAjUnWnh7LeCzVCAKFWt
                            |x1YueSkhbup9g745Qrrrxlzzeb662arekmDJcZespltpvORDR2ifXTeiur3bKHNJfRDE4w9SoS+B
                            |5qs4Ao00iHcd3cf3JNyYoH0slBZ0K3uN/I3NHYp3sRwI97VEAtnVSy6MvMRpAr6RqRShkWJwwzTf
                            |yb+yAdJlZ0vgGFW2EZX7US4SU1bm8/k5xT5OeEXqSi4MdXljCIlE4A9StJXT8bVjN1iNuVeqDJYM
                            |SoNjdhBjE9s3ZprYpg/00H1mvMgFs4KP3KGLqXFTkqiU4T9lo6wPGirTYk3TQMjLb60NWkM69qHV
                            |lQkgeSgQp03qvVIM+4LnmhD6U2J2n4zfTo50xQMFmKjBk4YW0wfUZ1g6jWEUVYCnE1AOcmfRhPQk
                            |3Yj0DHuTzsDm8v9wXCr5VQ/G5o8SY0s9CExfqq6FcL+4Sr9E2R4hdxPsiBDZcHxAbHs51Jvuk66G
                            |dSqvPZYnWb+9BelOHjqhE9GgpRGJkMvKqYdgAInjyNnK3PdvYyGf6AGUUy0Zn8yUQZAmpioesxBf
                            |oK8oFfgua61ggv5v8mcBNBjwWaJpbu3kUaLypwSi0lFuqhQD37BPCcHbBzsxBskS7lvszbQmPJvY
                            |G7B2ZWjbasdNGaG3Y2x/ANztWdp9kv0ucN20JxWFxliSyQDXvdlMi4ure92jVffzcrFfxRIgmvbQ
                            |vLriACn6YNAXF8MPSS16ZBJLtYqI8FJljgW1f2I5EdquxtWKBwgMeSbfZvCkDEjIVs0IK0+vOjYq
                            |sR9L3KRR9fExvEqo3KNjxMAIF/kB5N0g7hRO5EVjl16c4DkpukkzNz5qvoUDk8AHRejPSRJltJtR
                            |hZyDMRjT6X0pThA/CWnT6xQCOn4tt+4smvc445mlja8cFmaEFI/Wg4GBHI7PD0+OAQ700aGov6w7
                            |8raoJGqbpoF+cCAUer6NnBmhVR8A05ANn6dGL+tb8xpwNoxpLb9+LcLGvQLJ1eBLcF1y1ESYyGdZ
                            |3Tqpy70ywN8CYukpnjKRIgGnPM9mZ1hgXewHCSDBhfOCB6perWJbU3+cctr1JyVeHOGBSU9YP3LZ
                            |5neGeOef2aN5olFR3sTjNBQv/YNWFpjoYVZIt16ktz41+Hzhmj4q8rjS9poYn86oe0KB4AsJirEV
                            |aoLIwuwdgvjBGorZ9sHJVYmennPVngW80Qtz4TTn6IzK/Kt8xvmGAtky25aWWzW168qiov4oPOPA
                            |K6heqjgtOYwTU/zBVM+9NbkXLVINPMIogIzm1DvTuCNywLcgrn6RQAIa9pEKfr2TqLzZoEKM2q8e
                            |BzPOAjS6jZFWA0jBiJ46sPAUr5Y+GTmPMZlr2JxKOPByTM+ZnygmG9MyFJO/IOpz2KFJYjZSj4ds
                            |nisD4su9LUWmZs+X3/ztGQLOZsOeytd6elq6xVPhu7kOxYGFcBrsZLRNg0BHH+asXC43CjznpetL
                            |T4CC3gT6KS8eNPTSM8CVHoRYMbKw0hrpXKidcNTileCabvfqQbSqc4VvuypYkcLTBmRJ82W8zejp
                            |mnM39xsicTFknmzPr3l1RXTYZUHGvXman0DsGtcLFlb6vviGxuKo2j40gGvA++Xo23JjvFXkmqrQ
                            |9AVQfRq/cXjUTpQvHDOgjdeeCmO7fZY5e9myECF7JB1ZKH9Nw0Az01dGhvA2qsbk5GqMTK1Ty7af
                            |jgPEK9w8BH4n6r621yYTFlHtqlFUM/Mwd9zV3QRviX2zzWHSl7doQOJ8K9VydJ+ZyG52rzTNudHx
                            |Vmj0VtH3LVWDW1h3ImtVJ3kwjmtA9S9LDBwYOkK7HBTrZfwTlQRF9b2t3ljo4Fgz7Hs6F78LekBg
                            |IgIZKBBdj5iZHsgCJS/iHV723V1ba7iJQ+EmZVNoblu4ft/JLwL8SUYwhKqyLAukHdxPK1Bxy/Pz
                            |jq0VdLtf78xyaGhCxn5UPonLQTosx1wOX15HtPQBtCaqu/qnI8GNCol6L8iJfma27uzkY/t84t3x
                            |/j6v+dqkAOiSkzhJ2WwTmg09r3tmoisQMwTKj+bEIZKfCLKwmIAXJ3uL8rruhjHxqCACB03rBa21
                            |Mz1SYr3hw5rhaOvy3WCiFDjHaF/CQk6KNsCejUtSQNxLTw/iE13hkcQ1UrMR5ez939I/mYWIJisW
                            |VB6UVoloTrcRG9mjIQ3NNMKAxjazPwPHiAkBOVg+UPo4B2htq8021t6ms0luHr3x/VflX2Nn6rsM
                            |YbSXqjWirUtowryS/kt/JfKyMZw2BoW7sVHBegJhzWfNn75cEhjqtkbM37jdpyVWa3m6z4f//x1s
                            |/pH5YEtDBLg5Ssptnf8z2smPt0nwflbnxw8WqJNbDFS4uzoA9XlCBXAhwwd+lNKYBsFbkDkL+Bgp
                            |4cS7TmPcJvW1K2EPZqTMLwqghjbr5PL1CsnvzUiailfzf8xOTAZschkC1iHJEe0bjiDngrBy6q3b
                            |0ZWEIz2rEJm1eifCxua0CgI0EBETxG/6pKPvIZcC3h5DF0IwP2cv81Rfxr3RbPwXFWIbKUnDQHz+
                            |MmXW3LuJMkDDcn/KKosFyetr6udb7cfDFXsHgxZAAzsiKwghImDI0kWO32HfdXBQ4gAecONSUW2A
                            |2B0pgLD6lKoNC5LmgPoPaZsQw64cOqZrXGD5x/hzHVNWIHeMJHvEVpE76wZ5qesuNlG1VRHWV0Ra
                            |vPAOEoYAXEq9GD27o1eMmzQg2eArAfc3tB7PpR7Fd0pf1rr//4RCwC2A6VZELMIH9QB/9eMXPC8g
                            |RuxWUYyGXh7whdy6T1fCd4leAbeKArdu4cCY48Qv+1BpDiQf/TWJRBq2CwHKoz7RWWmb/a1Yz8VT
                            |xOh5LGEec1HM+pZiicYJufwfs69Y6kpkvog23AI+KTDnFcKlQoeru+L4XNBGBp07eG1/kh/akp8Q
                            |VBXyfb/BPeOeAquzwUzXMx7vL0IthURXJvDEwLv4+XWTrL5esBUzdAqd1bnIK4SaB/COdVsWl4iB
                            |P+n37Cos5XmIC7P0SNl1UL3Rg9IfXlaPRdRns0zbVZFe1U8a74vzkdMGv7a0B5612d0H08kQbXRS
                            |f9GMeR21+nd8X03q4cmQlqUYxG/8MQKkETHd1PABViFnPSMz+5SA67GYFXcF0SiVInhZi5pChZm3
                            |xJOMG87b9vPNjA+eqlsYlmDSSPg1dziJ5XZgutf9cCCfWZFByQK18bBBPhJvAT7EogzvSIuMwHUE
                            |jctqJzxQTwsTIY91g0Bx78m4RRCXunfDQez64udBvqjApSA/bBcuQ9JMx4iVG9ZqncIzKcckTqHm
                            |qtgTFlamVzBKfyQ8al5sX9fJnzoa7wtIuD7/gFRFQOQU2dHtutfwPwnrkLL4f/DR47NYZ5g91IT1
                            |QjuvJgD64IWrxJ229JtYj8+BA2qq/MIDHXIGylJXXCWhH1RrSexe2jJUSam6ubKEDpa6DzeBCWtY
                            |04QKVyaarXnKiELX8e6/sHgfGGS5k/gOvWEIT7uBxctFH5G+L7Gy9P+w0UeV+w/YUy5BpinHMboQ
                            |hTWala19aHXCd1tFhrbz1LO1x5Z03FbWZo+nfhtO/YPej4SBTqW6WDX5oVC5hO4TsId81s7yEdqm
                            |++g8iWyz69tn76ysBObJ5wek2A/PWgGOllcNvepVew+UnbP2QbCrWQYm3YB8k2VpOPcXGKtNQw+X
                            |5wvbFVp/7I+eVgNC3pH7BdnaUFREP0W7DTunPe/UMfcoDJIbgcWp+oMDacraLh9ETiWIHgJsCuJG
                            |lpeLUIAk3kMaiFhjbiozMq7t7pVOFBHeVejo4DJth6YTbCi+HQZQT9AJbHX5T9fAblztrofriS7u
                            |jEflUa/2NGc5MDrvVrHR6m2047ZY2N1p1Jt6Z2o3/UkjvsThXOj0fnwc93sJEJgJkOlXaNHbkHJ+
                            |Z9FfoJvGWH3yvUSTkkhcNZcvb9Oddry3MXHjOLYBSg601lGLi8NTAKaudc+6n0mfLiBj3F1r+hMD
                            |WaUSUcApDoKJJvfY1fOD/ZgxfLOrh8zetG2NBdSb8w2y5IstHp041giY+hYQeRe8TuOPQ+wQcjF3
                            |lXmRG7Kas/qEn15YL6op8hP1htW/Qem1o7UA7JVq3cYnNFzNQaNaRlU0cWZGAN6UUG/vPezL3FjE
                            |R/iyPqSxzra/sBdCIPyA94wX9qTaW7mShYUZy+hEj17E/4krsW+JZ5SEmcAl80gn4MKnl89drKjr
                            |VPB7JkOfQWUjj8kJ0NEH8WVg7pgwqnJDXEChcNzReiSnA76/nJTH45mMNFXPzMx29ZPsLwFinlG1
                            |2/YoaQ3A9wNq5rT9Ul3STmWT6UF3MKO1JkgXyXn7znaM4FNvZHFei45LEE9G03ygfNfSgi39Mr8X
                            |YP2cWp1Mx0hKdreFZkL/pXQ20ENEv+AoRPTYFgu5FBMSz7EIW8VIzrAgXpNVfHOZ+sUk74RGjLJA
                            |zkJcUlLQiW/lcIdkeA7axLJJiTjb7ulW0UA514MpgoTW7SLeiqpr3yXDJewduesYq1pUTY5+yrUQ
                            |oDUFaHn/gQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGh5/4EQoDUFjn7KtataVE0duesYJcMl7Iqq
                            |a9/W7SLegymChNFAOdfb7ulWskmJOHgO2sTlcIdkUtCJb85CXFJGjLJAxSTvhAAAAAEAAAAA
                            |]
            Monster ->  [str|Nzg3YTViOTdkMGVkYjBjMWJlNDRlZGMzODVkNjM4M2E5NTI2MWJmODJjZmM5YTI2MWMyZTM5YThh
                            |ZTdhCmJiZGYyYWJmOWFjOGYyNjliNmI5ZTJmMWM4ZTdlOTkzY2EzMTEwZDQwN2IzOTllN2M3NjYw
                            |OTBhZTYxOApiMGFlZWNlOTg2NjhkZGNkMTI4NjJmOGUyZWZhOGJjN2EwZGEwOTQxOTEyN2MxYzk2
                            |NDU1M2ZkNDg1ZTcKYjk4YWM3YWI3YmY1ZWU2MThjNDYyMzI4ODUxMzk3NmMwMmM5NjA0MWY4NjBl
                            |ZmVjMDA4YTdhYjM4MTc4CjFiZWZiNmJiN2E2ZTcyMDgyNzNlMWJmZGE5ZmRmYTgzNzgyYmM4ZTg1
                            |NDk4MDIxODVlNmMxNTQwZjRjMwpiYWQxM2E2YmRhMTk5NzRmMDczZDJmNTkzNThjODQ1NTViNGMy
                            |NGQwMDQ0MTkxZDE0YTM1MmI5ZjMzYjQKZGZkZDI2NDYzMTM3ZTA0ZTAxMzJiMzRlNmY4OTlkZDJh
                            |NWJkMTdiMzlmZGRkMzNiMmIxZGRlMDRhZDYwCjgyNzZmYjFjNGEzNzA0MDc1NDMyMzk2MGMyYzEz
                            |MWUxN2I0MGE4Y2RmMzA4ZmI1NzRlZTk3ZDNjYjc3ZgoyMDM5ZGE3NDAxY2Y3ZjU2M2JjMmM4NjE1
                            |MGQ4MzE4OWE1ZTA1OGEzMTY3OTYxZjI1ZTA5YzM4MTNiY2YKOTAzZDAxYWI0ZDM0OTZiNTY0ZWFi
                            |ZjUyYzFhMzhiYTBlZjQ3ZTg0MTkxZWZjYjBhOWM5MWQ1NzI1M2FhCjhkMmRmN2YwZTg0NjgxOWNk
                            |ZDQ5NzBlMzBjYmZkMzNhNDlkZjJkODI0NmM4MTdhZTMxOTkxNTk3MjgzZQo4NmU0NDcxM2M3ZTFk
                            |MWU3ZDY5Mzg2ZTNhODU0N2MzZDc1ZDMxZjFjNzQ3ZGU0ZGE1MmFjNzU2YjNlMzUKMzQzZTlmM2Ew
                            |NjcyOWU1NDUyYTMzNjA2N2ZlN2FjZTVjNDM4MmQ2NTIxMTNmMDVkN2Q4MzU4ZWVmYjM2CmNhNDFj
                            |MTZlNmViYTI0YTU4YTNhM2Q1NWNlNGE2MzM3N2E1ZDA2NTZiYWM4NGZlYzk2MzUwNjBkODc1OAox
                            |MzI4OWFkMzU4NWYyZjEzMmIyZWRlMjRjMzBhZmFjZWM1MmJhNDg4NmEwOWY4OWU5MTFlMWFlMThi
                            |ZmIKMjQyZDI4MTY5YjI0NGE4YzgwN2JkNWU4ZjBlOWMxZDRmNTNjOWEwMTFlZDFmZGU5ODY0ZTc0
                            |ODc3ZDAxCjdhNTQ1N2Q4ZTk5ZTRmMDBlNTBkMWZiM2M4MzI3NmYyMDkwODEzMzZjMWE0OWIxNjFm
                            |NmZhOWIyZjQxNwpmZGQ5NDA2NjliMTAyZWVjOGY0MzQ5NjliNTM3ZmYyZDBhMzJlZjhmMGQzYmUy
                            |NGM2MWM2NThiYWJhZjAKMjgyMTMyY2RhOTZmYTExOWI0NWVlZDY2ZDExOGQ0ZTg2NWZiNTBiZDdi
                            |MjkxNTE4OTIyZTBmYTFiMzc0CmY5Zjc4ZjkyMzAxODNlY2QxNDcxZGM0YTBhOWMzNTI0MDQ2Y2Vi
                            |YzM4ZTc3Y2RlZjRlZjRjNzExMjgzOQpjNGE1NTliMWQwNGUwMTY0MWNjZTFjOTA1ZGJiN2RiZjRm
                            |MTY0Nzg0ZTUzZDUxZDM5M2U1ZmQ1MDY1YTgKNzg3ZDM0MDA2ZWQ1YmRlYTY0ODlhMjQ4OWYxMGM4
                            |ZDg0MjRjMDQxYmU1YzE1MzJhNGMwZTc2NmFhN2FhCmZiYTJhMmYzNTM3MmE2YmI3NDA5MGQ1MGVk
                            |YjcyZTFhNDU4MjhmMWNkZTU5MGRhMjBlMDJjMzA1ZDg3MwpkZTU2OWJjMWY1MDVlN2M5NTJjMzMz
                            |ZDc3ZDY4ODM1ODljMGVmNTc0MjY4Y2M3ZjBiNWIzODExOGQ2ZDUKOWMyMWM4Y2QxYWM2M2Q2MDhj
                            |ZWIwYjdmZjk1ZjRhMjM2YmNiNmI2ZWY4Njk1NDE2YjYwNTg4MTkxZWI4CjBmNTUwMzQ0N2RhOTVl
                            |YTcyMmI2NjUxYzM5ODhlMDY1ZDZjYzg1YmFhOGIwZjIyOGQ1MDZiZjQzYzU3ZApkZmRmYTFkNWE3
                            |NDE0NTc4OGEzY2ZhMzcxZjc5NGYyNGNhNmViMTQ1NDJhZDM4MDc5MjQwNjE3MzI4ZGMKMzhiMmY2
                            |ZmVkZmQ3MzgzYTA0YjMwOTU2MjI1NjkyOTI0Y2UyNzVjZGIxZTM3YTc0ZmQxZjRmZDg5MjM1CmZl
                            |N2YyMjJiY2Y4Nzc2YTU2YzNjNWY2NGY0MjE1N2U0NWU1YzAxNmExOWViNGRmYjQyMzAzNmQ1ZmE1
                            |YQowNDI0YjRmZDkwNjQwOGQxNGVmMmYzMWYzMjQyNmUxZmQ5NmVkNWNkNTNmN2UwODA4NzY1MWQ5
                            |MTU5Y2UKOTY1ZWVjNGQ2Y2Y0YWNlMWUyNTI4N2RjYWIzZGQzYWNkZTM4NGM1OWU1ODYwYzNiNDM4
                            |MGQ2M2U0ZjdiCmVhZjU4N2UwMGZiNWEwMGRmNzE2YjFmNWI5YWI4NmM3NDk1M2YyMzJhNzA4YjQ1
                            |YjJhMGIyYTQ3NzI3YgozZjk2YmM4MjE5NDQ3N2Q1NDk5ODM1MmIxZjdjM2ZjZDJjOTBkYzNmNDI2
                            |YmRiNTlhNzBhMmM1NmQ2OGMKNzMwYTc2YmVlNzVlZDg5ZmFhNTE3ZWNlYTVkZGRjY2FhOTJiMWU1
                            |M2YzZDhiMzYyYWM0YWMyNjI3NWYxCmExNWIyN2EyNjRhZmU3MGExZmI4MGFhMzE0NjlkNDFmMWJi
                            |NWYxOGNjMjNmZDc4NzMwNGY2ZmRmZDc2ZAo3Y2NjOTYzMjA2MzhkZjJmNWVhNmY2YjRmZTYzMDUz
                            |MDhlZThjNjUyNDY0OWIyMjcwYzliYjMyMzQ5YzQKNTE2ZWFiZjg3NDcxMzkxNzIwMDQ5Njk4YjI2
                            |MmExNzc5NDc5N2FiYzgwYjUyMjk5M2Y1NmExMGQ3OGEzCmY3ZWY3NDMyMWNhMjVmNDYwN2Y3ZDE2
                            |OWU3MDBlYWIxYjE3NDEyMDNhODI3NzIxOTRlYTU1MGE1MDA1YQo5YzRhOGQzMDVhYzUxMTUxNjc2
                            |ZDA1Nzc0NjAzMGYxYmVhZDY3NWU5Y2U1NTJmMjc1YWYwNDAwOGZhZDYKZjExZTQxMTRmZmQ0Y2Zm
                            |Njk2NzFiNzZlZTZkM2FkNWY5YTgzMDcyNjkxY2QxZjYzYjg1MzJiNWJlMWU1CmMzYTI0NzVhOWUw
                            |ZjhkYzMzNjA3M2Q3N2FmOTBlYTBjNzdjZGZkYmFkOWI5YzM4OTg0OTNmODcxMDVjOAo4ZDQ3YWQz
                            |NjcyNDQzYmI2ZDYwY2JjNzcwNjllMDZjMDdiYjMzOTA4YWE3ZWQ0Y2NiOWQyZDlmY2Y2NGYKN2Q3
                            |ZjAzMjdhNjM5ZTVmNTc5MjZmODM5NzJjMDA4NmViNDMxOWIyNTI3ZGRmZmI1MDVmZWMyZWVhNjNi
                            |CmIyZmI1M2MzMzJmZjU5NDA0ZDE5NmVjZDNlMTAwOGU0ZmQ2MGZkYjAxZjUxNTQ3YzM0Mjc4MmVj
                            |ZTVmNwo3N2QxZjE2YmE1ZWZkZGQ1M2E0ODUwM2MyYjIwZTQ5ZmQ3NjdkZWNlMjNmOGMxODNjYjkw
                            |OWMwZmMxZTkKODhkMmEzN2RkYWZmZjkyMTEwNTMxZjlmMTllMDE4OTdmYTZkNGY0NzJlNWIzMDM1
                            |OTQ4NDRjOWZmNzZhCjEyMGNmNTkzYTI1ODE4ZTE1NjVlNjljZjY4YzM3Y2I1ZTE3MjBkM2FjOWEw
                            |MTk1MGMyMzdlZGJmNGE1ZgphYzc5ZjJkZDBjYjdhOWIzYzI0ZmQ3M2ZmNmIxZTkzMzExYzU1MTRk
                            |MjIyNTI5MDZlMmYzMmZkMjdlZGYKYzlkN2VjYzFlNDkwYTEzYzE5NjRmY2YwZDZmZWVhMmNhYjcz
                            |NmJmYTBiOTFmMzgyMTEzNzVlZjE5MzhjCmNkMWY2YTBjYjgyMDQyNmFkNGMxODVlZjYwMmVlNzBh
                            |M2NiMTEwMWYyYTYzMTg0NmI5NGI5M2IxZTZhZgo1YTYyMjhhOWYzNDFkNzQ0MDA4OWI1MzM4N2Ri
                            |NGJiZTZlYjI1NDE3NWQzMmMwNTlkZGY1YjFmMGU5OWUKNTU0YzE0MDUyMGY2ZDBiZTY1MmM5OWMz
                            |NzRkOGRhYmQ4ZDI0MDVkZTlhZThkMjk2YTZiZjE1NjJjNzljCjVmMDkyNTViYzg1Nzk1NGRkMWQ2
                            |ZWYyNmY0NjQ0MDU4YjMzMjI0ZjlmMzAxYzNmYWE2MWRjNzIwZGJjZQo4ZDdhZWZmY2IzOGMxNDgw
                            |ZjA0MmI5MTI1MzMyZmIxOTZiNzBiMjU4NTk5ZWE3YmVhYmI5ZmE3YmVhZjcKZDM4NjI5MDMzMGMx
                            |NzlmYmIwZDAzMTdiZmZjMWRiZjRmMmU4Y2I1NTliMzVhZDdlZjQzNDU1NTI1NTNlCmU0NzAzMGE5
                            |ZWQ3MDJjMmE3ZGQ5MjU4ZjhiYjIyMDM4OTc3OTQ1M2FmYzJhNWQ5YzAwYWYzMDRjY2JiOQo4ZTNi
                            |ZWVhMzM5YzA3NGRkMDE4MTJhZDM2Y2ZlNTcxNTAzOWE5MTJkZDIxYjQzM2Y3NWI3ZTgwNTk3ZjEK
                            |NmQ4YTUyOGI1Y2QwZDBkNzNhMTRlODVjM2JmNTM1YzE1YzRmZjllMzY3NjFmNjA4YmUzNzI3ZTQw
                            |NzFkCmY2MzA1ZmM0M2RhNjIyMjllNzllMWE2YTg5NGQwYzYxOTllNzI1MGQ4Yjc3Zjg3ZGU5NTgx
                            |MDAyNGY5YgpmMDU5NzRhNThjZjk3OTg3ZjM1ZWRhYTBmNmQ1YmJjNjI2ODBjZjNlNTI4NDkwY2Nj
                            |Y2FmZTZkNzcyMTQKMDMzMTgwODg1ODM5YjYxZDg3YzU5Y2QwY2UzYTA3NmNmZmFmNTE1NTU2ZjUy
                            |ZGNjYTM5ZjA1OTI1NTUwCjVjN2E0YzEyMjE1ZTQ2NzAwYzQxOTI0ZTNhM2VkYjY0MGU0NmE0OGNh
                            |MzFlMjU2YWRjZGYwNWU2ZTkzMAo2N2M4NmFjN2VkMTUyMGU1ZDQ3MjA3YTE2YTM2YmQ1ZTRlNjlh
                            |ZTg0YjBmMzM3OGRhMjQ2MTM0OWJiNjEKYzk1OGY5NTZkZWRhZWVmNmU2ZGNmZjQzMjkzY2NlNjY1
                            |NzQxMjU1MzBmZmRkOWNlY2FiMjFiYzFmYTg3CjM1OGQ2NzNiODM4N2Y2YTdkZjEzODc1ZDRjMDMy
                            |N2IyYTRjNTljNGJlNTc4MTRkMGI4ZDc5NzlhZjY5MgoyNGM2YmRkMTAyMjQyNmI4MzdjNDc0MTIx
                            |MzFhMzE1ZTQyYjhjMWUxMjhmZjg1NDZkOGFhZmI1NDA0ZWEKNjEwNDM5N2U1MDllMmQ4ZDYwODRj
                            |NDliYzMwMjcwMzdmMTM3NWI3NzkyNDY2ZThlZDcwZjg3MmZmZmIzCmNmYzhlMmZkZTM4ZjRiMzU4
                            |MThiZGE5Nzk5ZjRjMDNlNDVlYmE3NTg4MDliOTRlNjgxZjUxZWE0YTQ5MgoyMThlMWI3MzJlODE0
                            |YWE0ZjhkZDY4MmNjMjc3YWQ4MzI2YjBmZjkxMTBhMjNjODBiZjlkOGM2MGVmOTQKMDhhNzY2ODQ0
                            |MjA4MTVkNGQ4YzI3YTgzZjg1MmIxNTNkMGM3NDJkZDA1MTJjZTVmY2VkMmY4ODYwNWQxCjExNGE3
                            |NzQ1YTY3ZTUzMDhmMTA0ZDMyYTZiNDg0Yjk3ZTg0YTMyMzc1ODZkOGEzYzM1MTNkYzE2ZTY4OApj
                            |MWM1MTY5OGM0NjI5NDljZWZhODViNWQ3OTY3MjBkN2JmYjlmMzkyNzI2ZmI4YjE4YzliNDUwZmRj
                            |OTcKZWM5OTRlOWI2NWI0ODMzNmNkMTdiNzUxNjQ0YjAxMzAzODQwNWNjNTQ1OTExZDgzMTdmZDgw
                            |YTlhNGQyCjUyZDc0MjVjOWMyYWE3NDZkNTY3Y2Y5YTc1M2NkYzhkOWE5Y2Q5ZTg0ZDc0Yzc2YTI3
                            |NjBkODFkOWQ3YwpmNzZiNTQwMWE1MmJkODE2Y2ZjNDJiMTY0OTU5YTA0YjA0OGUzZTBkZmVmZTE4
                            |ZGEyYjEwNjBhN2Y2ODYKZDczMWRhZTQ4YTM2ODg3YWI4YmViMGViMTdhY2FiZjAxZGIwNTUwZGYw
                            |YTY5MDE0M2RjYzk3ZWIwYmZiCmMzNGE3MjdmYTc1NWJiZDk3YWVjZDk2MDM1YjgyZDJkMjk2OTE2
                            |ZTdkNDc2MzY0MTc1OWM3OWEwYzUyYQoxODA4OTA2ZjNkNDkyNzNlMzY2ODAyNjU4Yjk0NTJkNTUw
                            |MzE1YWViYjkxNTRlMWIxMmFhYzZmOWFkMDAKZDY5MTQzN2JjMjY3NjZjM2FmOGU5MjI2YzUyYjVj
                            |NTcyYThlNTZkOTI3YTA3ZmRhM2UwMTc2NjEzNzQyCmE4MTg3YmEwOGFjMzJlODJhZjA0MTdjOGQ0
                            |ZjMyY2ZjNzg5YWI4MWVhZWY2NWU0M2JkODJhODJlMTRhNQpkYTZlNzQxMWI5YmIzZmM2Y2VjZTli
                            |ZDQ0NWE1YWZhOWI1YWNjNGIyYzhjYTg0M2UxMzNkYjcyNWQxMGEKZTFhYTczMDA4NjQ0NWY0NjRm
                            |M2M0ZDc4ZWNmMzY2OGM1MGRkNDdhZTQyYTNjNzc5ZTEwM2E3YWU1ZTUxCmFlMjkwMTcxMTMwZDM2
                            |MGE0NjMzOTY4YmY3MTRlMmZiMThkY2Q1OTQxNmQyYjkzMzE3NmM4N2VhOWQxYgo0NDM1OTEwZWZm
                            |ZWY1NjU1NjNkOGZkNmJiYWZhY2MxNDYzMjljOGVmNWVjNDRjODZiMGFiMzEzYzU5MWUKM2UwMzc1
                            |Njg4MzBkYjYxZjc4MmJkZjFmMGQ5OTYxMGYwNTI0MGJkZjQ5MmFjYmI2YjY5MTEyYWIxZmY5CjZi
                            |YjExOTdiNDc0MjRiNGQ1ZWEzZjFhMTY2NTRmNDk5MzRmODYwMDRjODdmZTE3YTgxMjc2MjIyZTA3
                            |NAplYmVhMGZmZmE3NmI4NmMzM2E3OTFiZThmOGM2MDBkMzg1YTY5ZDE5YzI1NWEyOGJkNGQyZTA0
                            |M2Q4ZjgKMDAwNjRmOGMxMTY5ODI2ZGMyNjA2NWMyNDE5MWEzMTk4YjBlMDhhMWJiMzgxNGUzM2U4
                            |MjY2ZjYwYjhmCmNkYTk0NWRmOGIyMDliOTAxZjhhNmI2MDI2ZGE1MzcxZTlhMTM0ZGIyOTEwYzIx
                            |Mzk0NDc3ZTM2ODE3NwowNjE3YmM3NTExYzA3Y2Y0ZGM3OTJjOTI5M2Y2OGU4ZmE3MGVkNDM2OTkw
                            |NGMwMmJhYWM5ZTdhOWRiZjQKZDFmYWFjZGQxNGIxZmEzMmM1MTc0MmMwMjZiZGMyMTQ4MWRjZmRm
                            |NGMxOTk5NGVmYWQ1NTFmZmZiOTBkCmNhZTU1ODA4MTljZTE5MTYyYmY5YzY0NzJmODYzYzY1NDlk
                            |OTIwZWY1ZDZjMTNkYzEzYzAxY2Q4ZTc2YQo0N2ZlNDkzOGI5ZjI0MzU3MWM5NzRlM2UwZWQ5NjFm
                            |NjNhMTMyZTU5NjIzNDA4NmFkOTZjZmUwMjhhOTgKMDVhZjBmZjBjM2EwODU1ZGU4ZDE5ODY2NzAy
                            |NjE1MWYzMWJkYjRhMmZjNjhlYmZhYjNmNjdhODdlZDk1CjZlZTk0YWQwYTg5ZGUwNTYyZWZlZmU4
                            |MjYwMTkwYzgzNDE3OWMyNjk3N2E2MThkZTcyNjgyOGI5OTk2ZgplYzVjZDJmNzMxZWM3YzUzNDZi
                            |OGZjNTQwYWRjN2RkYWEyMzVlMWZjMmJlN2I4ODQ0M2UzZTQ3OGM4ODAKOTlmNGVhOWJkYTgyYzAx
                            |NWEyMzMwNmVlOWI5OTY5MDU2YjZmODQ2NWM2NzE3ZTE5YjE5NThlMDk5ZGNiCjhkM2E0MTBlNmU1
                            |ZDc1NmU3ZDk4NDY2YjhjMTdlNjM1OTVlZDdjNGM0OWRmNTdlOGUzNDg0MWQ1MWJkMQowZTcyN2Iw
                            |YjcwMTA0NTRjNWNjYzRhZDI0YjA3NGFjMDYzMTQ0MGE2OWEyZDQxMmZmZGVjMWY3OWFjYmMKOWQ1
                            |OGRiNGRlYTc4YTk5ZTljYTY0ZTRkZTU2NjczMTFkMWFkZWI5ZmRhODYxNGZmZjViZTYwMDQxMThh
                            |CjBkODQ0MGMxMGJhYTNmNzJjYjc5MWViZmFiNGM4YWJhNzZiYmY0NWY3ZmJjNGFmMGNmOGJkODBl
                            |ZjdhOAo5MWI3MDY0ZmIwYmNiYThkYTU0YjVhMjExNTk3ZDcwZGE3YzEzYWI3ZjM2MTBkNWRkYTJk
                            |MjIzNjI4Y2UKNmNiZTVhZDNkMGQ5OTY4MzA1YWQzYTUxZjJkMTNkMzRkMjBkMWViMmFjOWZhNjI2
                            |YTYzMDNhMDdiNWFlCjhhMjRiYzg3MGQyNzlhMDZlMTlkYWUyNTgyMzkzOGUxYmZjODkwNmZjMTUw
                            |Y2RmYTMyMTM1ZmFmOWIzNQo2N2I0N2M3ZDc4NzExMDMxZGNkNjBhNjA2MDYyNjZmMGIwNjBhOTYw
                            |NDczNzk3ODlmZmEzNGI1Mzc1NjkKODdlZmEwZWRhMGQ0MWQyZDk0MTE5MjMxMzU3NWQ5ZjU2OTc0
                            |ODQ1OGNhNjU2ZGVmODQxOTZhZTYxNzU2CmYyMjQ4OTRhNzNlYjc1MzUxYmI2Mjk2NGUxMzFhOWMx
                            |YmE1YTg5ZmM2NWNkMDA4N2IwMjFhNTBkYzdkYgo5YzBkM2I1OTVmZTMyMTJhZDJlNTk1NDQ1ZDc0
                            |ZjcxMjAxMmJhNWUzMDljNThiMmYxY2Q4ZjI1MzZhMDAKYjhlNGJiM2JkMmUzMDgyZWEwZjA0MjE2
                            |ZmY2NzdkZTAyMjdmODI0YTdhY2VjZTcxYmZlMDU2NzNmZTlhCjk5YTE4Mjg0NTE3NjRkYzZlMzM0
                            |OTA3ZGYwY2Q2OTg0YWQyNjE1ZjdiYmJmY2IxYmVhNTA2YzUyNWU3YQpmOTMzYWFlODA0M2QwNDYw
                            |Nzc3YzAxN2JiMzk5NGI0ZDNjMTJkNDQ4YTc4MjkxODY2MmQ3Njc4MDhmNTEKNTgxMmMwMGM1YjZj
                            |ZDgxYmVjMTJmYjgwY2YyMWMxNWU2MzAxZmVjNDlkYWE4ZWZjNjZmYTBiZTE2MmRkCjk3OTM3MzIz
                            |Mzk5OGI1NWU3ZTcyMjNiOTFlNzRkMWUwZjMzYTRiNTQxYmJkOWMyZDI5ZTQ5YjA2NzUxNwoyYjg0
                            |N2NkZTVlNDJjYmQ1ZDJjZGRjNjgzMzg5ZDc5ZGE0NzQ1MmNmNDcyYWZlNTM3ZmVlOTI0NTgyYmQK
                            |ZGJkNzU5NTU4MjE4YzUyY2ZkOWY2YTljZDE4MzhhYjUzNzEzYjNmMWU3NzUyNGM1YjJiMTU2M2Mx
                            |MzhlCjZiZDY1M2VkNjMwZmY2Mzg2YTZlOTc5MTZhMmQ4ZTM4MTQyMmEwZWQ3ODI5MzMwZWQ0ZmU2
                            |NTczYWRlYgo0ZmU4NTkwNzkzNDRjNDIzNDkyN2M5YzdmOTMwNjJkYWYwYWE5ZmYyODM2MjJlYWIy
                            |ZjFjNWE3ZmNlYjEKZjkzZGI2NzM0NTllYThkODZhYWVjMWVmNTgyZTVmZDU5OTg1YmEzNGMwMmNi
                            |NDlkYTcyZjNjNDcyZmVkCjAyZWE1NTVkYmNiNmU3ZTVkZmM3OTAxZmJhY2VkMTdmMTFhNjJkODY3
                            |Mjk1OGMyY2M3YTllNDFhZTRkMwpiZmQ0MDZkN2ViZjI2Njg3YzEyOGY1NjM1NTA1ZjQ4YTQ3YTlh
                            |YTRjNjI5OWMwMmFlNGI2MjFjZmJmN2EKN2MxMDhiNjY0ZjIwODM4MDFiZWZkYjQ0ZGI5NDIzMTE5
                            |ZDdmNDQzNTU4NzZiMDZjMWVkNDcyMGI1NzQ3CmVjM2ZkNjA2ZGYxNzhjMjIwNDk4YjhmZDEzMTI1
                            |MWY2NjVhNWYzMDE5N2I0NWVkYTQ2NGE2YTE0ZjUxMQoyZDYzYzgzN2U0YWFkZmRhMWQ2Y2NkNTI3
                            |NmNhNzM1Zjg1MWNjM2I5MThhMGYwZDhiYmM3NjM5NGRhNTAKZDk3MjhjNTJjOGJlMDQ5ZmExMmQz
                            |N2YwZWUwOGVmM2M3NmI3YjVjYTVhZjUyYTIyMTViNDJjMGRkZTc2CjVkZTZkNDFiM2M1ZmY1Njc3
                            |YTM3OGRmNmVhMjVmNTBiY2U4ZWFmZDJkYThmZmNkNGM1YjdlMjhmMWVjNwpkZmNhN2NmNDlmZjY0
                            |NjlhYzkyYzVlYjUwZjQ2OGI3Y2ZkNzhhZjRlYTJhOGYxMmE0OGZiOTM2OTBiM2IKODkxM2M3ZjY2
                            |N2NhNjgxOGU5OWYxZDA4ZDlhYTFkNGFmM2IxZjE2ZDExNzA1ZmZkYjViM2FmODRiM2EwCjY2MzNi
                            |YTE4OGI3MzM5Mzg0Njc1MDgwYzAyMzYwNTE2NDNiZDcyYzlhNzhlMzQwZjE4YWJjZjBlMmM3MAox
                            |NTg3NzJmNTc3NWRjMjYzMjMyNWRlZjdmZGFlNzFlNGU0MTk4YmY1ZTU0MWRmOTQ0MmIzZTE2OWQy
                            |OTYKODhmYmMzZmYzMjAzZmU2N2RlODMzZWNmZjA3NTk2YzIxOTg5ZjU4NDRkOGNkMGFlNjRhYmM2
                            |MDY1ODVmCjJiYjg5ZjY0MDBjNzhmYTI0YjkxNDA1MWMzNzdlNjEzMzJlMzU3YjMxYmE3Mjg4ZDdh
                            |ZjlhYmVjNTAzYgplNzhiMmUzYjU3OTg0NWNjMDIxMDJkN2U2ZTY3ODk1YjZmNDk4MDIyNGE5MmE2
                            |MmViZDJjYTkxNjdlZDgKNzNkYzY2ODk1MmQyYzJmMjliYWNkOGI4MWQ1NzIzZTU4OTY1N2M4ZWUx
                            |ODE4ZmJjYjRmMThjMjAxYmFlCjQ4ZWJiNGE1MjVjYTMxMWY0M2YwMDZiM2ZiODE3MzQ4NDc1ZWIx
                            |OTM5OWI4OWE1NDQxM2U4NzBkZWVkOQo2Y2E5NzkzMDRjNzQyMjZkYTcwZTMwMTQyODA3ZmZhNDJl
                            |YWEwOTk3NzEyZDFiYjZiN2EwODFjYTNiNDAKYjAwOTA1MmNiYjM1NWIyNDlkZTIwZjhiYjE3OTdl
                            |Zjk0NDA3NjRjNTgxY2RiYWJjNTViYjY3MjgzYzdmCjYyYWFiYWRiZmRiMjNiYjNmMzM0ZDhkZDE5
                            |OGMwYTU2NjkxODhkYWFmMzgwMWU1MzUwNTBhZjVhMzFkMgoyODE0NjA3MzQyYThlNmIzN2ViMGJm
                            |ZDZiM2E1MTU2MmQ4Mjc5NDNmMmRiYmQzYjA3YjA4OTgyODRlZmQKODAzZTJmMjkxYWYzNmU2YWQy
                            |Y2UxYTg1ZWY3MjZkNmI5NzVhZmJhMTFhNWRiZWFlYzg3Y2VjNWEwMDAwCjAwMDAwMDAwMDAwMDAw
                            |MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwYzg3Y2VjNWExYTVkYmVhZQo5NzVhZmJhMWVm
                            |NzI2ZDZiZDJjZTFhODUxYWYzNmU2YTgwM2UyZjI5OTgyODRlZmRkM2IwN2IwODk0M2YKMmRiYjE1
                            |NjJkODI3YmZkNmIzYTVlNmIzN2ViMDYwNzM0MmE4MzFkMjI4MTQ1MDUwYWY1YWYzODAxZTUzCjY5
                            |MTg4ZGFhMDAwMDAwMDEwMDAwMDAwMAo=
                            |]
            Map ->      [str|MDU0MGI2ZGRlMWI0NDQ3ODIwYzJhYTAzZGU5ZDFmNDQwMzBjYzc3MjhhMWFjNjM4NWY3OGNjNGVl
                            |Nzg4CjNlMzU4ZjlkMDNjMzM2YTY4YzJmY2U4MmViM2YxM2RmNDMyNTYzNDQ4YTI4M2IwNTJjYmU3
                            |NmEwM2QwYgo4MzM5OTI4MmIzNTc3YzM1NzBlNWZiMmFhZWY1MzYwNTlmZWVkMDg4ZmM1M2NkM2E4
                            |YTliZjllNTFiZTYKOTFlZWIzNTgxOGI4NTAzZDE5ZWU3NGQzNzcyZDNlZTAwYzdmNzhhMjgxZGRl
                            |NDYzYzNlMTY0YzU0ZmNhCjYzYmM2ODQxNGMyOWJkMmE0OGI2NjMxOTMyMzU3NzI5YmNmMTAyM2Ji
                            |MTM3MTFiZmE5YTUzOGRhMDYzZQpiMmZlMGEwYTI0N2RmNWMzNzI4YmVlMWIzZDY2YzZlOGRjOTFk
                            |ZjhkNmFkNWUxYjM4MjhjNTA4MzI5YjQKNTY1MTEzODNlMDBjMWVlYmM0OTJkNjMzZGZmZTZhODk3
                            |NjA0NDYxM2JlMTc3YmU1MjIyYjEyNmQ0M2IxCjcyYmMzOGM4NjY5ODkzNDY5YTZjMTViMGM4NGU1
                            |MWFlOWY4NjBkZjEwYmJlZmU4ZWY5MDA4NWRjNTQyZgo2YjQ2NmZiODk4OTlhNzExYjhlOWQ3OWUz
                            |MDZjNTViOTJlMjZlYWNiYzkwMDEwODlmZmRmNTUzNWE5OWEKNzhlMTcwOWRlZWQxOGVkYTY0Mzdm
                            |ZDc3ZWUzMzcxMTIzZWJhYzdhMGIzYjZlOTEzZjRhMTEzNmFjNzIzCmFjOTc1OWRjNDAxY2E3YmNi
                            |ZDNhYWU4NWM5MzBjYzcxNDVlN2NlOGNiMjlmYjNkZjNmZDNlODI2ODJmMAowZmQ1ZGUzYTMyOTQy
                            |NDdmZTBjZDcwMzljMTg2MzI3OGZkYTk1YTAzZjk2MGE2ZDE1NjFiM2ViZTJjMTIKZWQwNDVjOGM1
                            |MmQzYjhlOGM1NDVmYTE2N2M3MmM2NjJjY2Q2NDE1OGVkZTJmNjBhZTU0MWQ4Y2M2ZjViCmQ1ODcx
                            |ZDMxYWUyOGYyY2Y2ZDRlMWQ3NGRlNWU2ZjhkOWJmMGI5NDU2NDIxYWUxNTJkN2I4ZTAwMTVlNQow
                            |YzEzMzA1NzlmMjA0NDg4M2E3NjUxZDk5MGFiYmNkNDIxZDEzN2Y4M2FkYmE1YTA2ZTMwODYyZWQ2
                            |MWIKZmIzNmUxNzkxODAxYmU1ZWEyMDEyNjIwOTUwYjVlZmE5OGU0MjEwOWJkNDRmY2U0MGIxODU2
                            |MTA4ZTlmCjViMDEyZDM5NGRhOTE0ODYyMzAzMWZhMWNjMTljOTYwMjM5MzkwYTFjZjJiMDYxNTBi
                            |MmNkMWRlZjg1NgpkYzgzNDRlNDljYjcxYjcxNjQwNDUyZjk1MWRhOWFmYWEwNzEyZGRkOTgwZTk1
                            |MTI5NWZkZDBjZGM5OWQKZjYzYjQ0M2M0OTMzYTBjYjE2NTJjYmUyZWJjOWNkZjU1MDU1NWYwMTEy
                            |NmFiYjY2ZTk0ODUxNWQ4MjFkCmI5NzEzMjExMWMyZmZjZTcyNGRkZmFlNjdlNjRjMjY1ZDg1ZjFi
                            |NWEzOWM4NTg4N2I2YWE4NzE1OGQ5OQo3Yjc4ODVjNzI5NzE0NjkyYzhiZGZmZGY3OTlkYzI4OWI0
                            |YmZjNWZiMDcyMTM3ZjRkMmM0YjczY2NmYTIKMmJhNjkxNjNiZTAwNDNlNDNkNzEzYmZiZjE2ODBj
                            |NmFkNWM0MTJjZDI5NmU1MTRhNmY1MzFkZjlkNTY5CjdlMjBiOGE2MzI3ZGU0ZTliZjYyZTU3NGUz
                            |NjJiNjBlMTkwZDZiZjIzMTYyZWNmM2RkNDRhZDRiMzQyZgo5OGMzZjcwODJlNjI2N2M3YjIzNzc1
                            |ZGI1NzIwYWE1NDgxYWVhZjk0ODg3OTM0MDRlZjNjZTBiM2IwMWIKODU5YjU4MjMwNTI0Njg4ZmEx
                            |MDA1N2Q2YWM3ZjljNWMwYTdlZWRkMGM4NWMxM2I0YTFkNjA3OWM3ZTY2CjFlZjZjYmU5M2ExNjQ2
                            |MjZmOTgwYmJkOGJkMTMzOWFhYjQ3YjQwOTA0YjU5YzcyMDIyYTZmZTgzYWUxOQo0MjhhMDY2YTRh
                            |ZjRiZDc4OWVkODg1MTFhZGQzMzhmZDJmODg2MWFkMjg3MDNlZGE4NWJmOTc4ZDNiYTIKOTNmYWNm
                            |YTYxZjg5NmYzNmJhNzY0ODk2Y2YyYjU5NmNiYTdmMzEzYzkyZmFkOTBmNzAxNWQ4ZGU2ZDlkCjc2
                            |MDczMGU1MjllZDRmZjRmMDI0MTQ1ZjY1ZjkwZWU1OWI2ODAwMWFhZjIwMmU5NjQ1ZDE4MmMyNDA0
                            |NAphMDc4YjRlOGQ3N2U0ZWE5YzZhZDdiNTczYzBhZGVkZTQ5MGQ4NWMwYmY1MmI3YzJiZTkzOGY5
                            |N2M2YjQKY2FjMTE2M2RlZDIxODdjYmY1ZTAzYjVhOGE5OTJlZGM1ZjZkZGMwNzlmYjBjYTg0NDNi
                            |OGFhZjkxOTY2CjQ1YTM5MDkxYzY5NzJhMjUwYjA4NzQzODcxNzRkMjJlNWVhZTYxYWJlODAwOGU5
                            |NzMyZDlhN2Q0N2M0ZQoyOGNkMDYxZjM5OTVlMTNiOWE1MGU2Y2FlYTYzMDNjZTRlMjA3YzkzMTVj
                            |NmQ2OTkxNGVhYzJjMzQxNmUKNzg1ODI1MmU5NzM0Nzk0OWY1MjJjN2Y3NzZiZTZkMzUyMWFkZjFk
                            |MjgxNmU5ZmYwZTE5YTczYjAzMDVjCjE0Y2QxMmRkNzBlYjBlOWFkMDg5OTZjNjM0NWZmZWI3NGVj
                            |NTI3ODI3MTBlOGQwZjM2YTNlYTM3ZGNmNQo3YzE1ZDk2NTYwOTkwOGI3NDY1MTVjZWZiMTkyMDA0
                            |N2JkMWJhNzIyMjJkOGJkOWQ2OWI3ZmYzMDljNTAKNmFlYjU5ODc0ZGYyM2ViNzMxYmRmYmY4OTZi
                            |ZGZmNTViMzRmOTVmNjM0NDBiYjczNjUxZjViYjA2MjhlCjBmN2Y5ZmU3Y2E1Njc3MTA2MmVjYzky
                            |ZjkyYWJiNzczYjE0MjI3NThkOTE5ODIxNjJmNmE1ZTgxNDA0NApiMjMxZGI0NTUyOTgxMTZhNTFk
                            |OTg1MTdiMTBiZjc0OTZmNDNjY2E2M2Q1NDMyZTJkNzlkMzFhOWJhY2YKZTc3MzI1NDY4NThmNTgw
                            |OWMxMmNkN2Y4MzRkOGI1OWU4ZDA0NjlkMjBiN2U5YmJlMTU3MmZhZmNhOWE1CjYyMTEwMGNhMTQz
                            |NDUyZGI0NmY2ZWVjMmVjNTc5ODVkOTEzNzRkYmZiNTZmYTY5NGMwM2U5NWQ0OTUzZgoyNGM1YTYw
                            |MjRlMWU5ODI0NjI0MjRlYzZmYzM4ZTQyMmE1MDQ1M2M4YjQyMjZmNzJlYzgyYjg4YWI3YjYKNzdk
                            |MzFiMGMxM2M4MjAwNmUwZjE4Yjc1NTNmODhkZDQ5MThkZmUxNDFjZjk4YmQxZTBlYmE0MzM2ZmE3
                            |CjBkMjg1NGYxMzQ2YTU4YTZhNGNiNTFkZmM2MTJlMDE5NGJlODc1N2YzNTU1NzJjNDFhNmQ5NTZl
                            |ZDYzOQo3ZmM2Nzg3NWE3YjViMThjYzcyMzYyYzU5ZWI5ZTYwNDU1OTMzMDcyMTVhZTFjZTAyMzRk
                            |OTEyZDVjOTEKODRkYTUzYjJhNGU5ZGZmMzgyOTk1MzI3MjM1YTFhNzUxZGE0OTQ1OTMzODZlYjIw
                            |ZjFlZDFlNTZjZTNlCjZhYjZjMWU5NTNiYTUwMzVlMmM3ODEwZWMzOTcyZmZmODA5MDc5YjIwNTBm
                            |MzA5ZmRjNTNiY2E2NzBlNQo1NWEwOTQzMDBjY2QyYWFmMmRjNDE1YjRhZTczNzFlYTc1Y2Y4OGFm
                            |MjEwZjJiMDNhNzg4NzAzNzhiNjYKY2M1MjRiMDg4ODg2OGJlYzdlOWZkZTc1YzM2ZDYyOTA1OTZj
                            |NTliN2RiMWUwODMzZjNmZjViOWI4ZThlCmUzMDY3MmI2MzAxMGU5N2RmNjRmY2E3M2EzNjQzYWI2
                            |ZjdkMTU0NTUxMjk1MmIzNzNmM2ZkMmIwNGQzZQozNmQ2ZWI2MTZkZWFkZWFhNTcwMzc5YWM1YTVj
                            |OGRkYzBiOTYxNzZlYWMxNDg5YzQ3MDlhZDk5ZTIwOWUKYmZmMDc5MGQ1ZDk0NjEwZWY3ZTY4MTUw
                            |ZmM0MmQxMWM0ZTk4OTg3ODk3NTA5ZDYwY2U0YTg0NWQyOWVlCmM4Nzc2MTE1NjFkNjdlMGEwYWIx
                            |YTI4MmYyM2NhYWY1NzZlZWUzZTcwYmZkOTNmZGYxNmRiZjllZjdkNQo3Y2QwN2VjNGIwNDg4OTk4
                            |MmY1M2QyNWY5N2JiMjRjYmYyOGRlMzUxNDA5MDIwNjU2ODQ2NzZmZmQ4ZWMKZTU5MzEyMzI2OWM1
                            |ZDMwYTZkOTJiMjFjOTE0NDc4YTc4MGY5MjcyNzdkZDNkNDJhMWRmMTA4YTliZTVlCjNlZjI3ZDhh
                            |MDRkMzNlNTZlNjQwNGVlMWVhYzI1ZGIwZjBkYzY3YjQxZTBlMDA2NmZiNzEyM2UwMzk0OQo3OTRk
                            |NTZiNjgyMTE2Y2E1ZmQ0YTBlNTk4YTkxZGVhOGU5YTkyODJlMTE0ZWM3ZTZmZmJjNTA2NjViMmEK
                            |ZmVmYjFmMWI4MTFlY2FkODI3N2EwZmFmYTIwZjU1N2I0NDY2M2RjNTI4MWZkZmVjNTdiMzJhYzdj
                            |NTFkCjdlMDRiZTA2Y2EzMDIxOGI1NGIzN2ViYzQ2MmVjZDQ2OTBiYzVjZDlmOTJhMjkyMmNkZmI4
                            |ZTAwZTUyYwo0YmJjYjU1MWYzMTZmMTg3NzA4NjIzMDJkOGZjYWQ5NTRiZjIxN2QwZTNmNmUxMWJj
                            |OGViYzdhZWJhMWUKYjRmMTI5ZTg4MjgwYTgwYTFlMjdhYjdlZjliYzY1OTczNzFmNTliNTBmMTg1
                            |MTlhZWE0YzliYzk1ZDg4CjkxNDE0YTkxODFhZWJlMTk1MjhhYzJkMTQ5ZTUwMmQ5ZmViMTliMDNm
                            |ZGE5ZThlNTMxOGU0MjU0YzMxNwpjYmIzYTIxODNjYmVlYmNkNDkyYWRjYjk2OGNkODRiMDVkYzAy
                            |Yjg4NmRkN2E5YmI0N2ZjZWE3NTQyNDUKZDIwYmJhNzJjMTJiNzJjNTE1N2NjMjQxOTc2ZjNiN2Qy
                            |ODQ3YTkzMTcyMmM5NmMyODljZmQ0YjA4N2ZjCmIyNzg2ZDA5OWQxMTU1ZmQ4NmRhOWE5MzFjMzA4
                            |YmY1ZmVlYzI2ZGRhMGQyN2E3NWQzNjU3NWRhZjIwYgowOTlkNzEyMGJkMmViZWEwZDM3ZTI5NTk1
                            |N2EwOTg1NjRkYTI4MTcwNDM1N2NjNDlmMjM2ZGYwZTUyYWQKNGUzYWRkODU1ZmFiNzFhNzI1NDdh
                            |M2ViZWMyYjFjODMzODVjMTljZjc0NTllMjY5NDg1NGMwMDkwNWRlCmNhODEyY2Q5OGFhZjE0YzJh
                            |Y2QzZWVkYzNmZDVhYzE2MjQyODczYzRmMDFhZDBlYjM3NjgzYjIxN2U5OApkNmY3ZmNhN2M4YjAw
                            |ZjRhMjU2ZjlmOWNlYzVkM2E5ODM4YzE3OWNlOTE1YTlkMTBhZDQ0ODU1YTNhNjYKYjM4NDJjNTUx
                            |MTFmNDRkZTViYTIxMDVhMTlhODI4NGY1NjVkNDUwNzQwZDg4OTUwYzk4YmM1MWI1MjVmCjRkMWZi
                            |YjY0YzllNmJiMmI4MTcxY2U1MGIyOTBjMmNiODllMjg5MzNhZDRhZWQ2NjVjZjcwN2RiYWEzNQow
                            |YzFhNzQyMDQ5ZmQyYTEwY2I1ZTBhYzg1ODIxMjYzM2FkZTg1NjkwMTMwODdkNmM5YWZhMTY2YWJh
                            |N2YKMDc5ZTRkZmE0ZjY5OTA1OTQzZjYyZjU0YzhmN2ZmZDZiZDllZGNmYTgwNTc5YTY1ZjAwMGNk
                            |NzdkZGFlCjc2NmFjNmFjOTkyYjZmM2RkMmVlMTdmMTgzYmVjNGJmYjAzNWE5ZDg1NTY0NjBjNGRm
                            |ZmFiODRmNzA2YQo1ZWIzNDdmNzk0NDU5MjYyNjEyOGM1MTIyMTg5YTQ2NTAwMjA0MTI4MTVlYTEx
                            |MjMyYWY2YjJiMTJlMTkKNTY0ODBmNWQyNWNkZjA2YzY3OTI5NDYwZmViZWZjNThkYmE2Zjc0N2E3
                            |ZjE5NTY3MzZjNjBhNmZjODNiCjlmNzc3MjhiZjE5ZmY1NDYxM2RhZjFhNDc0ZmVlYTZiZTUzNmYz
                            |YzllOWQyZTc2MDZkOGMyNTQzZmI1NAo5NzM3Mzg2NjUwZTVkZjY4YjZmNDI2MzZlNThhMGRjNmI0
                            |YjNiNThhN2I2MTZjZTViMjMwZTcwM2FiZDMKNGU2Y2U4ZDBlNTFiNjM4ODFjZDRkM2Y3ZWVjNTBk
                            |ZTYzNjc1MGVmYmZhOWJjOWM4Nzg5MGMyMWI1M2EyCmZmNzhiNGMwOGQzMmJjNjkyYzI0NDc3MTFh
                            |N2FjMDE2MDE0OGQ2NWFlZDk1NDYwYjkwMmNiMjhhMmNhNwo5MTkxYWYxMGZmYmUzMzI4YTY0MWEw
                            |YTMwNDQ0NzUwNmE0YTUwMDcyZjgxNTBjZjkzZTlhNDllNjYwYWMKYTZiYWU2OTEzYzMzYWQzNzEw
                            |Y2M3YWJlZDQ0ZjdlMTNhOGUyMGYzOGJkMmNiNjY0Y2U0N2YzYjg0MTIxCjAyOGQyYTZhOTYyZjIy
                            |MWM0YTIwMjU4MDI1NTE2YWMxOGY0NDQzNzMxMDZiMmZiZTg2YjBmYmY4YWUwNAowYzJhM2M0YjU0
                            |MzQ1OGNkOTJlZmJlOTcwNzA2YzcyYjcxY2Q0ZDc3NmFhZjU3OGI1MWNiYmYyYTUyNzEKYzQ5N2E4
                            |ZGRhMzFhZWQ2YTc5ODYxYjhkYjYxNmQ3MWM2MjljODQ0ZmJhOTE2NzBmM2JjODg0NGIxYjZmCjhj
                            |MzVkYWY5ZGJmMWNhYjcxMWVmODQyZWRkOTUyNjVkY2NkZWQyMGY4NWMwMDdkYzY3MDhjOWM0ZTQw
                            |OQphYWI4YWVmMGYzNmE2YTQ0NDA5Y2RiNjJjOTdjZGM0NDZhNTljYmY3MGE1MGIxMjYyYzdhODI1
                            |OGJlZGQKM2UzMzEzNzY2ZWFlZWIyYjU1NjM0YzI3OThiNjY1Mjk5MjMyYWFmODkxNTA1ZDBhY2Iw
                            |ZTI0YzY4NmIzCmJkYjE3ZjA0YTYzYzAzZmY4ZThjODY2MzNhYzEyZTZlMWY4ZGJiMzM0NzJlNTQy
                            |YmFjN2FiMmI1ZGQyYQpjODhmNzE3MWI3MDhjNTMxMjg2YTUyNzMxZjJlMTAyYmQ1ZTgzMzk1MzQ3
                            |ZjFiNGQ3YTUyMGZhODZiZmIKNzM3YTlkODMyNDQ5MmU5NzQ0MzExNzk2NTE0MGZhYzMzYmM3Y2U2
                            |YmI1MDZmOGM4NWM3NzY5Yjg4Njc0CmNhOWJkNDg3MTZmYjc1NWVjNDA4ZjM3ZjRlM2UzYjg4NzEx
                            |ZDE3YTA0MjNkY2Q3Yjg3ZjVkMjgzN2M4Ygo5MDVkMzg1YjRiZTFjNzRiYWQ3Mzg3NGY3NWE1YmE4
                            |MjhmNmM3Yzg3M2M2MWY5ODQzYjM2YTA0YWI1ODQKNzU2ODgwYTFkZjUwZDkyYzZlZjc1MzYxYTA1
                            |Yjc1NmNlYWMyOTI5NjQyNjU2ZjEyMmM5NjlhMWNiNjI2CmViZmZjZWYyMGQ0ZWJmYTQxZGYwNDAz
                            |NWZjYTJiM2M4NjM0MTY5ZTkzMGEyMmY5ZjgzNmNkMTZkMDViNwozYzlkYjU5ZDI2NDc3NTA1NTI3
                            |ZTc5YWE1NDI0ZTNhZTI4MWVkNjg1NmI1YTlkY2EwYTM2OGJlNmU3OWMKYTU0NWY3NzlmYTUzNzBm
                            |ZjljMzZmMTU5NmJlOWUwZGE4NTdhNWQ0Y2UzYjc5YjQ5OTI5ZjkxYWVhYjcwCjk2NjlhMTQ3YjVi
                            |ZDY5ZDI2NTYzYTk1YTZmNTI0Y2VkMzQ5ZTA5MzA4NDdlNDdkODEyNTc3YTc1YzNlYQpjOGFiYWI2
                            |YjU3NDhkNWVmYjk5NTkxNjhlMmFhMDk2MDc2M2FmMDMyZTE0ZmYxNTU4YTg3MDA1NmM4MmQKZTRj
                            |MWQ0Njk2OTUyMmRjOWI2MzY0ZDRjZDU1ZGMyODIwMTJlNjE0ZGFhYWQ1YTg1MTk5NWNmZWJmOWVm
                            |CjYzM2ZhMmI1ZGU3OGUzODZlZGIwNTU0ZDBiNTlkNTllMDE5NTczNWMyODU2OWM1YTc4M2I1ODg3
                            |ZWYwYgoyODcyODM4YTRkMGFkNDA5NzgzMGQ0NDZmMWRkYzExYWQ0NmNhNzI0OGQxY2VmNzE3ZjFm
                            |YzhmMjQ0NzYKM2FhMmI1YjljY2UwNzVkMTIxYWEzNTk0MDhhNDA4Nzg4NmYwMTA0YjNlMjhhODE1
                            |MTMxNTlmYjdhMDVkCjVmYjIwYTJjNzJlMmFhNzgyOWI4Y2JhZmE1OTZmZTkzYzUwNzNkMTlmNjU1
                            |ZGU5Mjg5NzA5YmEyMzIxMQpjMGM4NjQzZGNkMTUyMGYxYWYzZjUzNzAxZjY4NjVlN2NmNzg2ZTFk
                            |MDMzNmQ3MGMzODNkODE4YzI4MGEKNzMxNjA0ZTEzMjIxNzAxYTQyZTQ0ZjEzNDFiY2Y3ZTVlZjI3
                            |OWU5MDI1ZTlkZmRjZmJkNjI2NWVlZGI2CjExMzJhOWQ1ZTIyZGU4NDBjOGVlM2EyNzMyMDNlNTE5
                            |ZWVhYzJiMjZlZjljMGI0ZGZmMDZkNDFkNjM0OQo4ZDdhYzA0MGVjZTc4NzBiOTllYzMzOTdjMWRh
                            |MGI0MDA3ZjQzYjU1ZjEwYjE5ZmJmZDUzMTJlMzM1YTAKN2M1OTIzYmQ0MjM4YTU1YTdhNGRkMzcy
                            |MWNhNjQyODQ2OWY5ODRiMzM5MjQzYTI5MjY1MTc4M2U4MjZiCjczYzMxODE1NTQ2MTc5MmQyNmY5
                            |NTc2ZTVhZjBiZTNjMjhhNzQyNDI3MzBiMzdkMjYyMTBhM2ZiNjA3MgoyMjMzMDk0MmUyMGI1YTIy
                            |MmU0ZmJlZWY5MmE2MTQ3MWFmYjhjYzA2N2Y2OGMxMjlmY2M0Nzg0Njk4MWIKZWM1ZGNhNzYzYmUx
                            |NjBiOWUzMmQzNjZiNzcxMDgzZDhlM2Q0NDc4N2MwN2NmNGY3ODEwNTFjYTdmMmUzCjA2MGFmMTUz
                            |YmY2ZGI5NTgwMzMxYjExMGNlYzIwZDYyZDI4ZGI4ZjhiZTMwODU5MTg3ZjI0YWU5ZDU1OQpmNmNk
                            |YTE1NWUyZDliMGYzNzc5N2Y5OWExYzE4MTQ0N2NlMWExY2QxZmRkODE5MDQ4ODU2YzhkNGIzNDYK
                            |ZGM2ZTA5ODljYzlhMTE4OWY0MjcyMjE1ZTEzNDBkYjkzODQ3ZjA0OTUxZWViMWNjMGQ1MzhkMmQ5
                            |MGE3CmUyNGE3YzEwNDY3YzZkMzEwOGQzOTA0NTU0MzRmOTlmNDQ0YzQ0MzUzNDgzZGJhYjYyZDEw
                            |MTJiMmI1YQo4NGRjMzIyNDAxODM1NzY2YzVhODZjZWZmZmU0ZjY1MDg0YTZjZWI5OGI1YjNlMGI0
                            |ZWJiYTc4OGZiYzUKYTJlMWUwOTllYjE4NmI2MDEwZjFiYjIzZjY4OWUyZTM0YTcyYWNhYmNiYjc4
                            |NjdmODJmNjkyOTg4OTg0CjUzMWZiOTQ4NTk0NmNjYTY1YjY4MDM1YjUyYjM3MDBkZTJjOTkxMDVm
                            |M2E1ZjM0YjNlNDI5MDZiNjBjOAo3MGI2ODAxZjg2ZjBjMjYzMGY5YjViNGMzZjM3MWFkZjRkMjY1
                            |NGNkOTg2ZGRjNDc0NTQwZTgwMDhjNGMKZDg3YzBjYzY1ZjQ5YjI2MTliMDEwODA4YWI3NDk2YmQ2
                            |MWI0ZjBmNzM3ZDZkYTA0NTA3MjcyYWU5NDY1CmZiMTNkZDZkYjg0MTZmOTFiYTE4MGIyZjY2ZWJl
                            |MTM5MjExZTA0NzE3M2QwODYyOTg2OTlkNGE3OTk1Ygo0ZjAwNTY2ODlhMTgyODFkZjMxOWUwYzEz
                            |MTdiYWEzNTVlYzhjNWU3OWU1NjMxNmUyZjQzNmZlOTlhNmIKMGJhMGZhYjJkNDZhNmFkYWZkMjU1
                            |MzU4NjU2NjIxMjZjZTE4ZjY4Y2YwMjk5ZGZlYzg3MWQyNTgzOGExCmNiMmQzMmMyNjQwNWNmYzc5
                            |OTlhNGM0NzUxMzU3MDU5OWQwZGFmNzk3OTFiYzhjMDc3ZjlkMDU3M2VlNgo4Yzg0ZmU2YWYwYmEz
                            |Mzk2YThlMjc5ZDRhMDFlMWNmMjEyZTQwYzA3NWJlYWYzNTkyYjMyNWMxOWViZjYKYmJkMjMyZThm
                            |ZmY3NGUyMDNkMjZmYTgwMzFlOTU1ZGEyYWIzMWYzZTQxY2E3NmNjMmYzZDllZDk0Y2VhCjkwY2Yz
                            |ZGUwZTBhNzIzZmZkNDZiZDQzOGEwMzMwNjFjMjY1ZTYxODAwMTg5NDJiZjRhYWVkZjdlMGJjNQpm
                            |YjEwZmM1NmY0M2I0ZTc1NzllMzM2ZTY1YTJhMTI5NDQxMGY4ODA5MzM0MzQ1NzEyMzY1OWI1ZjYy
                            |ZTgKZDU2ZjdkYTNmMzFhZGI2N2RiOTg1NDFkODQxM2QzZjM3ZjMwYzlhNTM5MGM3YjBmMTA5NDY0
                            |NzY4YmNiCmRkZDhkNTI5MGRjODYwMzNhYzY3NzVmNmExNjEyMzNjNTViZWNiMjRmZjdmMzMxZDg0
                            |OWNmNmRhYjdmMApkODViOWQ1Nzk4YTU5ODUwZDBjMjg5MTdkN2RjNjI3YzNlYzMwMjJhZjY1ODZi
                            |MDRiMmJkZDY4NjM5ZTkKNDIxM2M3YzNjMGRjMGQ2ZTc1YTJlMWM4NTk4NWQ1YjMzYjM1M2EyN2Fi
                            |MWJhOGNlYzk0NWNkNzliM2YwCjE2NTg2OTFhMmVlM2Y1M2EyMzJlOWY4ZWU0MTQzNjY3N2IxMGE5
                            |ZGQzZmUwMjgyMGEwNmI2MmYxNGVkOAo2ZDNjNTZlOWQ1M2M2NzNjZmM5Zjc4MzkzYjhmNzkxYTBk
                            |ZjAxY2E2MmYzZDcxZWNkZTk4NDk2YjQzYzQKZTU2ZjFlZDZjNWE4NDBmYzkxZGVjNjU2OTUzYTEz
                            |MTQzNGVkZDU3YTU4ZjJjNjI0NDRiMTdjMmZlODY2CjE4MWQ2YjliOGZhOTRhM2VjMzg4MDNmZWY0
                            |MzFlNDA5OTI5MTY2ZmYxZWIzNjlmZDE2MDMyNGUwN2ZjYQphZWE2ZDNlNDg3MzJhMWMyZWRlN2Uy
                            |N2NhMzFkZGJiOTM1ZGJhNGY2OGU0NDZlOGNmOTQ0ZGIwMzhkNGIKNDc5Y2JhMTM5OTNhN2RkYTFk
                            |YjcxNmJiZDYxOTUyNzVmYjQ0YjIwMGZjODcwOGQzODkyNzM1YWUwMDAwCjAwMDAwMDAwMDAwMDAw
                            |MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwODkyNzM1YWVmYzg3MDhkMwpmYjQ0YjIwMGQ2
                            |MTk1Mjc1MWRiNzE2YmI5OTNhN2RkYTQ3OWNiYTEzZGIwMzhkNGI2ZThjZjk0NGE0ZjYKOGU0NGRi
                            |YjkzNWRiZTI3Y2EzMWRhMWMyZWRlN2QzZTQ4NzMyN2ZjYWFlYTYxNjAzMjRlMDFlYjM2OWZkCjky
                            |OTE2NmZmMDAwMDAwMDEwMDAwMDAwMAo=
                            |]
            Thing ->    [str|YTc0YTMzYWIyZWYzOTEzOWRmM2Y0YmE0YmJhNmM1NjEzNzRjZDNhZDU1ODI5MjA5YTIyYTg3Nzc1
                            |MGU5Cjc2M2M4OTRmNzlkODI1NTgwZGNiODYxZTkwOWM1NTljNTRhMzFhZDY4ODQ1NmUyMTM4MmY2
                            |OTJhY2VkZQpiYTc0OGQwOTY0YWM4NGQ2ZDllODJmOWExYzAxZmJkYWE3MzNmZjc5YTY4ZDA2ZDc3
                            |NjNjOTdlNTEzYjUKN2ViNDM0NmVmMDUxZDY4ZmMxNjVjYmNkYjYyYTM4NGJjZjk2NDE5NWU0OThm
                            |YjQ1ZjhjYzc0ODljYzdiCjYxNjRlMWU3OTM4OTMyZTUyOTcwYzQzNmQ5NGFmN2Y2NGUxNmQyMDJj
                            |ZDdhMWNlOTkzYTRlZTkwZWE0MQo5YTllYTRlNDVmN2YzOWVmNjJlMDUyODFhMTRhYTZjOWU5ZDlm
                            |YjI3MzY4OWZhMDRhYjE3MDA1OTFlMDIKYWU2ZGZiZTBjYWMwMDc0ZTQzOTI0M2EzZTYwMDU4ZmI0
                            |YTI4N2FiYzIyZWNkZGMwMmQ1MzhiZDAxYjQ2CmQ4MTg0M2M0OTUwYWY2ZWRmMjk4ZTJlZmU5YjYz
                            |N2Y2ZDc1YjZlODdlNzY2MGJmMmFiYTIxYTBjMTJjOAozNmQ4MjViYTllZTU1MDQ4NjhkZDkxMDQ0
                            |MWMwYzAyYzliMWZmOTg1YjAyZGYzNDRlNzI1Y2ZlNmU2MjcKMmQxMDZmMzAwODJkNGJkNzkyODE2
                            |ZDkzMzQzMjcyYzNhZGQ5OGI4MzhmMzIxYmY2M2IyZDMwNDc5MjFhCjEzZTc4MWRjODgxNTYwYzlh
                            |MTAyNmMxNTk2OTNlY2QyMjc1MTIyYWYxNjU1NjExYTI3MmZkMTEyYTlmOAozYTA2MWY3MzkwNzFm
                            |YzkxMzA4N2FlMGU2MWVlYTlmMzI4ODZkYmYxYmY5MWJhOGIwZWEzNDVhZjg4YTQKYzcwZjI1YTIy
                            |YzQwYzZiMTg5ZDBiOTEyNDU1MzRlNzk5YmE5MTliOGJkNGY5MWY5MDAxZDg0NjBkMDI1CmM3MzI3
                            |MzQwYzYzODhhNzNjNzA3NmZmMzg3OWY0ZWE1NDVjM2Y0YTQ1ZGMxNzdmNzQ3ZjIyZDc1ZjViMQo1
                            |Njk1ZjExMmUyZTJiZGZmMDQ3MmQwNDNmY2EwN2Y3N2FkZWFmMjM4MmU5MDI5NGNmOGE4ODczY2M5
                            |NTAKYmJjNDc5ZjkyNzVkYzgxNjUxYWU4OTNhYTRlMWNkM2UzMGE5YzczNTVmNzIxNmFmNDZmYmZm
                            |NGNkMjI4CjA3NGQxZGY5MTg5MDEwYjY3NmY3NzZhMDg3MTliOGUxZjFhYjJlNTRjNmY2ZWViYWE1
                            |MDcyMDMyY2Y0Nwo4NTBmMDBiMmU0ZjI0YWIyNjEyMWI0OWI1ZDc3MDg0YjhhYWZhYjAxODEzZDA0
                            |MGQ4ODU2NDA0MDBjZTEKZWQ5MDM4ZGNlNjgzODhlYzMxNjExOWIwNjhiODFiZjM2MmNmZjhmZTkx
                            |OTk5OGIxODRjYjcxZTQ1MzA3CjkzYWFlM2FjNzJjZTcwNmExOThlYzUyOGY1ODRhODJmMTQzNDdi
                            |ZDZhMGY3MTUzM2NkMDBjNDhhMTk4Mwo0MzRiNTFkNDgwYTE4ZDI3YWMyNmEzOWYzNzY2MzdkOTFk
                            |Mjc1YWZhMjYwYTQxNzRlM2ViMWQxMWQ0NjcKN2RjYzk3MDE5NGE3MGNiNzVlMTQ2YWE0NTY0YWVi
                            |NmEyNWJiNzNkYTU5N2JjMjIzNzBjODgxMzUwYjFmCjdmNDMxMTgwZWY5MDEyMzJjOTFiMzUwM2I3
                            |NzEzMjcwYTU1ZGY2ODgwNDgzYzdkMzQwZjM2ZmZkMzYxZgo1NDU2MmFiYTUyNTBiNDM5NWQwNjU2
                            |Y2Y1OTI1NzRhYTI5MDlkNTcwNjI0NTM5M2ZiYjg0ZmRkZWJhNmIKZTgxNWQwOWY3MmY2OTNjYmNk
                            |OTg3MmFlMWM3YzZkOGY2ZjNjZDI5MTlkMTRiZTI3MjMxNTM2MjMzNDNiCmYzMWRiMWExODNmNWFk
                            |OTA5YzNkZGEyMWYwOGU2OGViMWIwYzgyYmE1ZjA1N2VhOWZiNjA3ZjkwYjI5ZAo2ZGYzNTAyNWMx
                            |YzdmYzlmZDQxYzllY2ZjMzk2ZDJiODllYzgyMzYyOTFkMWU4MzNiNmI4YmE4N2Q0OWUKOWFmNTM5
                            |NmM2ODI5YWZjY2ZhZDAyYmFhNGNhYzI2ZTQ5MDBhOWMxYzk0NWMxNGVhMDUyMzk1NjRiMWYyCjBk
                            |YzAxYTFhMTc2NjY4OWFkOTY5N2MyM2RmMzBmMDk4YzNlYThkZGEyN2ExN2E3Nzc0ZWI5MDZjYzg4
                            |MwpjMzQxNGNkYzQ5ZGY4MjkwOWM3ODdiMTM3MDMyZTdhZGJjYmFmOTVkNTk3MWM4YzJmOWU3NTAw
                            |ZGIxMWUKOWI1YmMxNmViM2UyNWE3OTM1YTM5MWI2NGMxMzk3Y2YwOWJhNzU0NWQ1OGQ4NmZlZTg4
                            |MjZmOGM3Nzg0CjIzYmRmZjdmOTc1NmQ4M2Q4MGE0MzJiNDgzOWRkYjRkODZjMjIwZjcwYzkzODAz
                            |MDI4NDZhN2FkMmJlMwowMjlmMjQwZDJkNmI0YTU1NTY4MzMwNmVkOGE5ZDYxMjMzZmIyNTkwMTMx
                            |MzMwOTVlOWZmNzdiMjAxZTEKNGMzY2UzNTI4YTI1NzYxNjk5M2Y1M2ZiZjY5MGFlYjE0MzcxYzRi
                            |NWIwZDU0NmE5ZTg2MjI2MDhjOWU2Cjk3MDE2YjdjYmU5MGEzZDFjNzA0YTVmODYyZGYyNGI2M2U5
                            |MzdiZDE3NWRkNzFkZWE5ODhhMzY4NjBkZAo4YzIyMDBkYjVhY2NmYzU0NzEwYTZmODFkYzIxOWE4
                            |MGVkZmUxYzUwOTk3ZjkzNDhmMGYyOWI0YTViYTMKYjQ2MzQwNzYwYzFhYWM1MzU0MDNmY2NkZGJk
                            |ZTAxYmYzNGE0MTQ0MGExYTNjZTJjOTZhNjNmMWE3ODg1CjlkMjFiYzI2NWI1YzkwNjNlMjM5NjVi
                            |NGEyMmEyYTc2ZmE5Zjc5NGRkYTExZjBjODVkMThmZmNiYTk0Ngo4NTU0ZTc2NTBhMDc3NjkwMDQ1
                            |NmQ1NzllNGYyNmEyM2Y3ZDUwYzc4YTgyN2Q2Y2ZhZjhiYTg4YmEwMDgKZTBmZDg5Y2JmN2ZhZmI0
                            |OTdjY2EwZjcxYjdjYTQzYTE3Mjg4YzJkZmU1OWFjZjRiMDNkY2Y5NWUxOGFjCjM1ZGE1NWI0YTRi
                            |OWQ1YmVjNzA0MmRmOGUyYjI1ZTcwYTI2YTFmNzczZTUyYzNiNzkyNzFlYmNhMDQ3Zgo2NTY1OTcx
                            |NWNkOTZjY2QyNTVjYjlmN2U5MWNmMzZhNzZjMTdmYWE2Yzk4NjQ5YjBkZDI5ODhjMzFkNzkKYWZl
                            |ZTI1ZTBhODlkMjM1OGNmMjRhNTliOTI0MTg3N2IwNDM4MGEwNzQyNTk4MzJlNzIwOTFmNzYwMTgx
                            |CjMwZDg1MDE1OTg2MzhkODM0MmFkOWNjNTI4OWIzOGFlZjVjOGRjMjlkNTI0ZmU2NDBmYTVkYWRm
                            |ODBlNwo0ZDk3ODQ5MzMxZmI3NDBkZDQ2ZGYwZmQ1NzBjNWVmNjNkZjRiNDJmMzIzNmY1NTk0MzIz
                            |NmFhMGUyNGMKOGE2MzYxNzc4YzU1MGQyZTYxY2Q4ZjhmZjg4MTM5ZGE2Njg2ZDYyZTgyZDllN2Nh
                            |MjFhYWI2N2MwNmY2CjQ0NDQzNTJhMmQzODhhNzExNGZhNjNjMGE1NGZhNWIxOGE5Y2FhNDA0YjBh
                            |NDBjNTZmZjc0YzFlZjE0ZAowNWY4OWMxZTBiM2NjMmM3OTc4NmU1Y2FkZDllOTY0NzU4YzgwZGU3
                            |ODFlMWY3OWQ3ODFkMDE2ZmJmMmIKNWRiYjQzNDMxNGQ1OWU3MmE1ZTFiMjIzMDc2ODQ5ODZlNGJi
                            |Y2Y0MjRkMjJhOWY3YmU5OTk1NzY3NDg2CjUzOTUyMWI1ZTRiMzg4M2I1MDY3ZDc2NTlmMzQyYmQy
                            |NDQ4YTk5YWQzMzY4NjcyZDFlN2ZiYjIxNmI2NgpkZmM1NTIwYTFmZDg0ODA5MjliZmE0YmI5YmE4
                            |NWEzYjc4ZmFlNmY2ZGVlODY1MzAyMGJkYTM1NjFjNzUKNWIxNjU1MTY4ODU5N2E4MzZmZjU3NTdk
                            |YTk1NDM2NDI4M2M3ODMwZDRkM2YyYTQ1MTFkMzhhNjEyM2I2CmYyNWQxNDM5ZmZiNzU4YWY3Nzg0
                            |NTJhMTFiODJkYjM0NGU1NzY2OTVhYmZjNzI3NTY2MjFlNzBmNjNlMgpmMjYwNzQyN2FjZDRkYjli
                            |MWI4YzdiZjAxZDdlMDI4ZDllYmVlYmJhZmYxYjI2MGNiYjNiZTAwOGQzZWUKNGMyYmU1N2I0N2U3
                            |NjU4NmU4NmFlYTk3NDU0ZDlhYjljMzE4YWRlMDRiOTk2ZWExMTRkNTcxZWRjN2U5Cjg4MGE5OTEz
                            |Y2JhMGNiMjRkYmYyNDcxYjFmZmQzNDExNzA5Nzk2Y2NlNjFjNzQxOWI3MzU3ZmE0YjY0ZQpjNTE5
                            |MThiMmMzNmRmMzZhYjFlNTJhNmE5YTZiMWYwMmU5NWQyODU2NmI5MDQ0Y2EyMzRmMjJiMzJiZDQK
                            |MTFkY2U5Mzk0YjU1Njk0YmE2MWRlMDE3NjQwMTE2ZjVkYmEzOWQ5YmFlNmFlYjJkY2ViMWVkODY1
                            |NDEyCmY2NGJlYWFiMTAzZTQ4NWVmYWJlNWExNGQ3M2UyNDdlMWYyZWZkODc1MDI2MjE5NjYyOWE4
                            |MTg0YWIzOQo3NmMwZDBhODMxMzYwNjU2ZDY3OGZmM2ZjMGNhZDEzMjNhZWUyYzJiNTU2YTcyOTE2
                            |OTZlZjA5ODQ3NmUKMTU0OTc4NGQ3M2RiN2FlZmEyODRiOGRkMzFjNDBlODc2MDM0ZWRiY2FmNTUz
                            |ZjJjOWZkYTBhNGM2NzlkCjA4MWFjYTA1MmUwOWEyMTFiNGUyMWJiMmRjYmQ1NjYwYTdlYzg4NWMw
                            |MDI1MWJlMWYzOTZjNGNjMzRjOQpkZDAzNzY1OTJhNDRjMWEzZjI5Yzk0Yjg1M2MxNGNhNWY4Mjdk
                            |MTNlM2U5OWVlMjE4OGE5OGUwM2IyNWYKN2FhYjJkM2JhMmI5NDVhMTM3YmU3ZTU3NzYzYjdhOTk0
                            |ZDIxZmVjYjhlYTIxYTJlZGNlODU3MGNjM2I5CjU1MGE4MDRmODYxMjlmNjEyYWFlMjhlYWZiNjY2
                            |M2I1ZTg2NTg5YzAzYmI1Y2M0NjJmYjFjYmJiY2UxNQpiZWFmZDFmMzM4ZDc0ZTA1NDAzMTgyMWVl
                            |OTliNmI5MTA3M2VjN2JjMWIzMWYyNzg3ODBmZmQzNzM5MjAKZjU4Y2E4ZWM0NGU3MTY3ZmUwNjA5
                            |ZDU2NjdmMTcxODJlNDI4NTAyZjJlOWRhN2M5MjdiZWIzNDg4NzQxCjgwN2EzODlmYzBlMzNkYWNl
                            |ZWM2MjA1YzZkY2ZiYjBkNjRkZTU0Y2U4NzkwOTVhMmYxZDQ5OWNkYjdkZQo5ZGRmYjI2MTNkMjlj
                            |MzA0NDE4NmRhNjNkNDMxYTBlYjY0MmZlNDRmNWRjZjc1YmQ0Njg2MmQwNDViZjAKMDAxMDI2YWRm
                            |YjU4NmZkOWYzNjcwYzBiZDAxNGY4MGVjMzgyYTQ0YzhmZjNkNDczMzM1MjAyOWU4YmQ4CjA4YTgy
                            |MjIyMDkxZTQyZjE5YjUxMDNlNjQzNGJiZWRhNzQzMTExOWI4NzI2NjFjMzZkMDk4N2Q5NWJhZApi
                            |N2QzYjIzNTdjNDZkMzE2MDRjMDQzNjUyOGM4NTExNTcwYmIxZTQwOTc5N2ViYTljOThmYjg0NWUw
                            |MzAKZDg2YmMwYTA4YzlkZTAzYWRhNDNlYTk5OTRjZjUxYmE1ZmFhYWZmMWViMWZiN2UwZjM3Nzlj
                            |Y2EzNmMzCmQ3N2I1MjdlODkzZTE2Mzk1YTU0NDUxNTI5NjY0NmJlZDQ5YzExOTE1ZTFmMzFiNGM4
                            |NTRiYjRkNTk4YgoyNGZhMTE1ODkxMTAxZDAxNjVkZDAwNWJhODA3YzBhNmNlMzhlNTBjOGE2MDM5
                            |ZjM1Nzg3MGVlYWM2MDYKYjRiMTk5NTM2NWIxNjlhYjAzODM1ODEyZTY3ZmU4ZjA4YmJjZjdjMjUy
                            |MmJjODViOGQ1MmM0OGMwODE2CjIzZWQ1YjU1YmYwYzMwYmE3OGRmMzY1MzcxMDc3YjkxM2Q2MDk2
                            |NjU1NTEzZDk1YmQxNTVkOGFmNDAyNgphZjFiMTE1N2IyMTU1MGJiMDE2ZTU4Y2VhMzg2Y2VkY2Zj
                            |MTg5MWVjODhmMzFlZWQwOTI5ZTgxYWU3NjkKNjhiYTZhOWExN2MxZWE4NWIxMTFhZjkyNGViYjMy
                            |NGVlMWQ3ZWJmY2M3YzNkNTg5OTM5ZTQ4ZjNhMWJlCjA1NDBiMDU5ZjdjNWM2NTY0NmFkYmExOGFl
                            |NDk4NTRkMDZiN2UwYTc5MGM2NGI5MmYwODM2ODFmZjk3Zgo0YTYzNWY4NzBiNjgzM2M4ZTEyMzhm
                            |NDg0NWNjOWQ3NjE3YjQyZjM4YzA2NDY3MDY1MDY0YTlhYTUzYWUKZjlkYzAxZGQ1YzRhYzM5MzVj
                            |ZmQxMzg1NWJlNjVjYmVmMmQ1ZjJhMzFmOGI2MWRlMTI4ZTFhMTY0MGNiCjhiMjY3OTZhYjhmNmVi
                            |ZjQ2NWQwZDE5ZDEzODQ4ZGEzN2Q4NTI4ZDNkNmYzMzhjMTcwZTZkMWY1OTRkZgo4ZGJjYTc3ZmUy
                            |OTM1ZjI0YmVlMDJlNTAwN2U5YWE0Nzc5Y2I2OGEwM2ZmMWI5M2IzNzRmODY0ZWUxNGEKOTExNzJh
                            |ZmZiNzZjMGE4NDM1ODQ1YWY1OWY4ZWU0NWY2MTJhY2ZlNWE4Njk2ODVjOTE1MGFhOWJkNzE5Cjg1
                            |NTU5MDBjM2U3ZDE0NWM0NWRiZjY3YmE1N2JkY2I4ZTdjNTdkNDMyODA1ZmE5NDQ4YmNkNWE5MjUz
                            |Zgo5NDE2OWQ2Y2QzYWY4NTE0ZGZjM2JjYjMxYTJlYjYxNWNjNmQxZDgwMDlkOGU4NGE5ZDY5OTc3
                            |YWEzNzMKOTZkOGE5ZDhiN2ZiMzg3ZWNiZGQyMzA0NTYxM2Q3NGRjMmJiY2Q5ZTMwZTBiY2I2MWRi
                            |MmJkNTZiNGVkCjU4MDI3ZjNjOGI5ZGE2MTU0MDVlNmYxMmZjMzBlN2U0MjRiOTc5ZDI5ZmRlNDFj
                            |NWE4ZmVlNDdmMTAwOQowMGFlZWJlYWU5ZmUyOGQ2M2I3ZTE0YjgwOTAwNjc1ZjVjZTRkODdiMGQ1
                            |YTIyNzNjZjVmOWUxY2Q1MzQKYmVjZWFjYWU2ZTczZmZjNzc5YmVkMDE0NjViYzJiMDZkZjkzNjNl
                            |NmRkMjgxOTNlMmQ3ZTdlM2U1OThjCjA5OTYxNjgxYzc0YjJiNDU5YTg2Yzg0OWNjNTdmODUyNDMx
                            |NGRmNWEyNjg1NWVhZjdjN2I5YTViNTcxYgpkZjIyOGRhY2U5MzBjZmEwOGQ4ZWNjYmQwNmEwMzIw
                            |YzIwYjAwMTI4NjBlYjkxY2JlM2MxYWMxYzA4YTgKYTg0NWJlODZlY2Q2MWYwNWM5N2VmMDVlZDdh
                            |Yjk0NDE5ZmE5NGM0NTJmMGVlYmE5YjVmM2UxOWIxYmU2CjFmOTU4ZjAwODEwYTNmNmMwY2JmY2U1
                            |OGUyMmZlZmI4Nzk5YTViMTk2YTQ1MTRhYmQxNzVjNDg1ODVkYgo3MWI0ZTk0M2I3ODA1MjQxNzMy
                            |OThjMmM4NWUzMDE0MTNhOGNmM2M0ZTJjMzk0ZjY0OTg5MWE2YTliZTUKNDNkZmFjNzE3MTdlOWYw
                            |MWFjYzNkYjczMzYwZWMyNGM2YTA5ZWNlZGYwN2ZlNGJjZGEwNmZjMWQ5MGFkCjcyMTIwZTY0N2Ni
                            |NjQ2ZmU0OGEzYzUzNzNmNWJlYTlmMGI2ODY0M2YyOWZiMTgyZGFiZTlmOWQyN2U2NwphYzM0ZGYx
                            |MTYzZDhmOWMyOGI4ODYxNTcxZDk4ZjRlZjU3Y2RlMWU1NDViOTRmMTkyZTg3ODJmMWU5M2UKNTY3
                            |M2IyYjA2OTE0YTBiZTlhYjg4Y2Q1MzViNGVhYWVjNWMzMGUxYWU2YzNhN2MwNmM1MDIxMzU5MDIw
                            |CjY3YzY0MzMxOTI3OWI2MmE5MDJhZGI3OTU5NTFlOWM3MDI5Y2FhYjRiMjkyZWVjNWY3ZDlkNjM5
                            |NmQzOQpiZWFkZThiYTZiM2E1MDE1YjcyZWI1NTdjNTUwODUzNDE4NWZlYWNkNTVlZDUwNTdkZWJi
                            |NmI4NDQzYzkKOWRhZTc2NWFmODM4NTYyMzZkNjRiMDlkNWQ4YjcyODVmYTkxYjQ4ZjMyNjBjYzM1
                            |NzY4ODY4Y2E1ZmRjCjQ2ODE4ZGM4YTA2NDk3YjE3MTNiOTFmMDA3NjNmOWMxNzMzMWQzNTQ2MDk5
                            |OTIyNGI4Njg5OWUyNDZkNwoxMzQ2ZDRlNGNmN2Y1NjQ5NGFlMGUxOGU3OWU1YzY2ODVmZDAzNTY4
                            |YmEyMzJiMWIwMzQ4NTIwNTNjZDkKMzJmNDViZmY5NzRkMTRjMzFmZTc3ZjQ1YmQwYjA5ZDRjZDM2
                            |N2ZiZDM0OTc5ZmM3YWZjNWQ2YTYyZjEwCmFlNTZmY2UzMTU5ZWUyNjkwYTU0ZGMxNzBlZDI5ZDBh
                            |M2VkNzZjMzBiNzllNmE3ZjZhNDdkODJmMmZlOApjNzY0Y2JlOTVhYjhmZmU5YjkyZTY1ODNkM2Nl
                            |ZDQ5MmY5YzY0OTk1ODhkMWFiYWRlNGQxZWFkNWIzOWQKZmEzZmY4NjBlNTc0YzBhY2VlOWQyMjI4
                            |OWJhZjllNTcxYjcwMTFjYWM5ZGFmNGFlMGY0ODc3NzE3Mzc2CjYxODU3NDcxYTBhYzA1YTFhYjU2
                            |OTdlZTRhYWM4ZTM0M2I1M2EwMGI0YmI1ZTQwM2E0NDQ5YTYzNWRmMQplODA2YzFjZjU4Y2E1NjNl
                            |MTRhODI3YmMxMmQxZWVlM2RkNzRmMTM2OGEyNzBlYTUzN2MwYmIwOGM0YjEKMDZkOTMwOTlkM2Yy
                            |OGUwY2QyOGU2ODdjMDk2MmRhNDQ3YmJkMmZhNTI4NmRhOTYxNmRkMWM0NTQ0ZmU3CjNjNzNhMDJj
                            |MGY4NTdmYTM0NTNiOTNiNzc5YTkwOTZhMzVmYzA2ZWQ4N2ExNDI0MGZhNmVmYmRiMGE1NAo5MWQ1
                            |YmY4YjQwNmI2NWRiODM1NGU0NWY0YTY3N2MwZWViNTJjY2Y2NjFiNjBmNjNkOGQ1ZTBmYmNiOGQK
                            |NmMxYWY2MjlmYjI0ZmU0YmJmZTc2Y2JhNjJjMDkwYjk4ZmFkNjJkMTk3ZmM0NTk0MjNmNzMyNjA4
                            |NWFkCjdkMDg2M2NiMDAxZGFhN2JmNmU2NDE2MWNjMDkwZDJkNTMyYmY3YzY1NzAyY2VmOWJmZWE2
                            |ZjNkNjA2NAo4YWFjN2ViNGQxOGQ3YjVkMDgwNzlkOGVkMjc5M2QwMjIyMGI0NWI3NjllZDlhZWUw
                            |YWFkNzMwMTRkYzMKYWNhZWE3MWI2M2Q0ZGE4NzYzZjM5YTM2ZmJjMjZlN2QxMjM2NDM2ZTQwY2Y4
                            |YjUyNmQ4MzU5ZWEwNWIyCmYyNGU1NGYzMDBiZmYyMmMwYTMwYjhjZTRlZjBjZDY4ZGMwNDg0OGEy
                            |OTkxN2ZiYTk3YjY1YmRkYzRmNwo5ZmFmY2NmN2RlOTQ2ODE0NzMwOTJlMTY1Njk5NjBmYjMzNjg0
                            |OGEyMzI3NDRkZDE1NWFjOTAwODI1NGEKYWIyMzI2N2Y0MjcxYzVhNTk5ODZiOTgwYzc3MDJiNDY4
                            |YTZkMDYwMGIzZjJkNDZkOThiMDdiYzY3ODBiCjNjYzQ0MjJjMWNiOTBkY2JlYWEzOWEwN2M0Mjhj
                            |MDk2ZjA0ZWI1MmI1ZWZiMzdiYzc0MTAwNWMwZTQ5Ygo4OWU3NWQ3MDk3NmNkZWIxMmIxOGQ0NmMz
                            |ZWYxNjI4ZjNjZDNlYzQ2NTdiZTcyZGI2NTgwZTc2NTRmODkKYzNhZGQzZmE0NmI2NTdjOTA5ZjA1
                            |OWQwMDhlYTQ2NGQwZDYzOTA5ZTRlY2ZmNDczYzEyN2ZmYTYzNzFmCjZkMWFjMmNjZTUzYzk4MzM4
                            |Zjc0YTU1NTM4MmNmYzdmZWNmZjA4ZDc2YjYwOTcxZGRhNWZkMWUzNDM5OQpmOWJlNjNhZTVmOGU2
                            |MTUzMmVkMTViM2E0MDRmYzUwNjRlOGQ0OWYwNjA5NmViODVjMTkyZTlhNGUyMTAKNTNjY2VmNmFi
                            |MGViZDEwNzc4ODRiNTk1MTc4ODJiNjZiZThjMWI5ZjZiMjc0YWEzYWI5ZTczMTgzMTczCmVhNDZk
                            |NWQyNmFkNjg0ZWVjYThhODc3YTEzYTYyNzMzNzUyNGIyNTczNmYzODIxYjNjZmYwMTE2ZmI0NQo3
                            |ZDkwYjgwMzdhZTY2MmE5OTQ4ZmY0NGJhYmMzNDU1NjAyYzQyYTU3ZjFjMWI2ZjYyZTg3NGQwZjdh
                            |YTMKOThiMTRiZmE5MzVlZTI3NGQ3MWJhZTQ1ZmVkZDhkM2M2NDE1NmY3YWUxODYyZmM1ZmM1NTE2
                            |NGYzZDIzCjY5Y2RlNzcwNzM4MWQwMDliZWE3NjVmMjM3ZDJkZTRiMGRmOTE4NTNlMWQ3NTU0ZWJh
                            |ZWRjMTdhZWE0Nwo4ZWNmYWIxNmIzYjU1ZmM5NzE5Y2Q5NTk5ZGU4ODViY2QzM2VlNDI4YmRjZTZm
                            |YmNiM2M4ZmI5NzRlMTEKNzEyOTU4MDU2ZGJlMDNhYWNhMzZkYjFiOTM5OTBmY2Q1Njc3ZjMwNGQy
                            |YThlMTIyZTcxMWMwY2I2ZTI4CjJhMmJmN2U1YzExOTM0ZTNiZTI4NjFmNGY5ZTJjZWY5ZjUzNTRh
                            |MzgxM2JhNGVlNmYxN2NmMjlhOTNmNgplMzBhNjNjMWIxOWJkODBjN2UwMmYwYmY4NWI5NjIzYWQ1
                            |OTU4OGE0ZWMxMWQ1ODVhOTgzZThlZjczM2UKYWU3MmEwNzE3MjEwYTk5YjQ1Y2QyMzNhOGU5ZGVi
                            |ZGQ4OGY3MDI3YjE3MmIyOTNjZmUxNDk5NTJlNGRjCmM2NjdhNDJiNWZmZTRlYmFlN2MyNDVjYzgy
                            |MmY4NTg0YTAxMzBhZmI4NDkyNDExMGUzOWFlYzJjNWYzOQo0Y2VlMTIxZDkxNzBlNDk5ZGZlYTll
                            |Y2YxMGI0OGM0YjQwNjI4MTE1Y2Y1N2NkNjlkNjBmOTQ0NmRiYzEKYzYyZmI5MWQzODViOTU4YTBj
                            |ZTIzN2UyZWUyMjg2YTdjN2ZjNDgzODY4YWNkMWY2NTdhODJiZjUwMDAwCjAwMDAwMDAwMDAwMDAw
                            |MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwNTdhODJiZjU2OGFjZDFmNgpjN2ZjNDgzOGVl
                            |MjI4NmE3MGNlMjM3ZTIzODViOTU4YWM2MmZiOTFkOTQ0NmRiYzFjZDY5ZDYwZjgxMTUKY2Y1Nzhj
                            |NGI0MDYyOWVjZjEwYjRlNDk5ZGZlYTEyMWQ5MTcwNWYzOTRjZWVlMzlhZWMyYzg0OTI0MTEwCmEw
                            |MTMwYWZiMDAwMDAwMDEwMDAwMDAwMQo=
                            |]
            SoundSet -> [str|YmRiNzg1YzIyODExZTE2NDQ3ZDM0ZWQ4NGMxMTg5OTk4YzgyNGYyYmM2NDhiYjQ2YmM3YmRjOWVk
                            |MWQ4CmNlNzFlMjg1YjMyYmZiZDg5YzMwZjA3YjM1MjgzMTFiNTljMWEzYTJkNTQwMmUwYzkwNGY3
                            |N2I3ZTAzZAo5YzE5Y2EwZmJmYmU4N2Q4NmIyZmI3NGM2OTczOTY4MjM0MGMyMjIwNWEyMjU3ZmU2
                            |YzcwN2NiMjE5NmYKNjViODhkNjZhYTQ2MzQ4OTU0MTJkNjIwZWQ5MTk4MTlkZWZjNWQyMmJjMTQx
                            |YmVhNjA3ZWIxNzU4NDcwCmEwZWQ3ZDQ4MDc5OTQ1MTBhMjc0MWIzZmE0NmRhYTAwZGQ3NDAxNDcx
                            |ZDM2YWExMGQyNDUyN2MwOTQ1OAo5NTBiNmRlMDAzYzE3MDA5YzA0ZmNlNTQyYWRkNzI0NDk1NWI0
                            |YzkyYmQ3MjFiMDQzMmQyYjZhNjEzYWYKOTc4ODkwMzRlZDQzYmY0ZTM4YTY2NjMwM2I3ZTg4OWU5
                            |MGI2MjIwOTM0YjlmOGYwNDhmNDIzNTVkN2IwCjBmYTM4ZWQ4YzBiODBiNWU1YWFlNjZiZmMzNDE5
                            |NWNlM2VlOTg1YTY2ODk1NGIxYzZiZWY5Y2EwOTRiZAowMmIzNzUyMDQ2ZTEyNGMyMGFiZGZhMmU1
                            |NmZjMzNlNzQwYTc0ZmNlOWI2NDFiMDZiMWY1MTQ0OTQzOTMKYTQ2OGNkM2NlZTFmNTdiYTY1ODc3
                            |MzA2NGExYWIxN2YxZjk2OTc3NDI5MTdmNDgwYzgxZWY3NDQyZTFhCjA5YWM4NjlmYzdiNTA4NDky
                            |NWI1ZmNlMzcyZTEwOGQ2MTkyZGZmYjkyZTI4YzhkOTU4ZjQ5NDQwZDU0Mwo4OTQ3YTRjZDgyNWJh
                            |MGJkYjRhMTlmNTVhZGRiNzVmZWE0NmE1N2U5ZWZhOTRjMzdlOTczYTEyOWIyYTUKMjAxZjY5YWY5
                            |ZDg5NTM4YjcxNjZhNGRiYTliMWIyNGQ4NjFlYmE4YjdiNTZiMDRlMTc1YTAzNzRmNmQ0CjZhMWNh
                            |NzVkNjVhYTQwZDkwODNjYTFlNTM2OTU3NTEzNjc4ZDAyOWYzMGZhMWI3NjViYTc3MjY3NmJjZgpl
                            |NTYyZWJmYTg2MjMyNjViM2M2MmVjODQxMzEyNmM3ZDNiNmE2OWU3Njc0MGE4ZWFhMGQ4MmM4MGJl
                            |Y2EKN2JhMGRlZWMwZTFlN2MzMDk0ZDllNTkzZjU3YTQ1Y2QwNWExODFkZjY2YThlMmZjYWFlOGYx
                            |OWIyZmZlCmM2NzBiYjk0NDQ2ZjBkZDFkYzUxOTRjOTI2ODAzOTM4NzZhOWEzZTEzNzFlNDRlMzFj
                            |MjIxYzc5MzU0Ngo0YjlmYjI1MjU2NDMwYjhhMzUxNTZhYzk1ODgxY2ZjMThlZjQzZGQ5N2U2Zjdj
                            |YTg1MjU2MzQ3OWFlYWIKYTY2NzcyZDVjMmRkMjM1NDdjMTc5NGM3NDdkYzVhMTY5ZDAxZTUyMmQx
                            |ZGExNzVjM2Y5ZjhiNmM5Njg1CmRkZDA1ZTlmYmZhZDIyMWUzODA4MjM0YTJiOTFmOTIwNmI4N2Ew
                            |NzZmOWZkZDY4NjBiMDEyOWFlOGI0OQo2YWNmNmIzNDdhOTE0NmExNGEwYzc0YmEwODNjM2JmZjhl
                            |YTllZTQ2MTJkY2E3ODI1YjBlZjljNjU3MjMKNzE1ODMyMDY5MjBjZjRmMjllZTIwNWM5OGFmYWEz
                            |OTJlYTU4OTNlNWZiNDBmZDBlZjk0NDJhMWFjOGMzCjQxZmUyZDkyNDRkODc3YmM3YmRlODc5ZmFj
                            |NzM2YmJjMjEwNmMzOGY0ZTY5NjUyZmEwNzYxYzMxYzczNQo5YmEwYTljMTZjYzFkZTBhN2QxOWZi
                            |MzM3MDg4YWIyNDc5N2NhMTM0ZTVmYTViMzE3MDIzYzZiMTcwODkKNWY5NTA0MWJhYmMwNTE2NzYx
                            |ZGJmMWNmN2VhMDBjY2RiNjMyZTE3ODMwMGQxZDAyYWEwNGE4ZDdjNmVhCjdkOTIwNDA3MzIwMGQy
                            |YWU3MmY4OGM2MjUwOTM4MDE4MTU5YjdhNGNmNjU5NTNjYmM0ODA5OTc5ZjdiNgozYmIzZDY0ZjJk
                            |M2YxNDg3MTgyMDZjYWM1NzEwNDQ4OWY4NTMxYmRiNGEyNTc4NmJiMDNmNDA5MDQ1YjYKOTdlZTM5
                            |NzMwY2FlOTcxMDZmZWE3YTc2YmFmNzZiNDcwNDBiNDlmY2I2NGNmNmI0Yjc2MDlkODgxNzE4CjJm
                            |OGNhOGE5YjRjM2ViNzkwMmE1MDZiNjliYjk3ZWEwNDg1NDdhYzIwMDdiMTNlMTI1NDhjYjIyNjRj
                            |NgphNWE5Y2MyYjBjNmQ0MzAyZWYyNjAxNmRkMzA4M2EyMWMwZjBkNmM1YmY1ZWYxY2VkODQyZTk4
                            |YWExN2IKZTk4OGJlMGQ1YTg0YzdhYzcwYzhkY2VjMjA3NTIyNDE4YmM4NzI2N2ViZjMwYjNmZTRi
                            |NjMwNDdjYThkCmQwNDg5MDgyMzA1NjcyODlhNWQ4ZTY5NWIwNjYyZDc1YzllYTU0ZjQ3YzJlOTI4
                            |YmNjNGUxNjE5OWJhZgpkNGE0ZGFhZDMxOTY1M2MyNjU2MDFlYjY4ZGFhMTRlNzFmZGJjNmY3Mzkx
                            |ZWYyNDI2MDg1NzUyOTZmM2YKYjUyZGY0NjRlOWRhZjg5Y2E5OGMxMmQwNDIzM2MxZTY0YTdkZDkz
                            |NDM4ODMxNWU4ODNiODhjNjhkY2Q2CjQ5ZDQ1Njc1MjhhYWU4MjU5ZDUzMGVmYzgxOGY0NmJjNTE3
                            |ZGVmMGI5MjA4ZGY1MDZkMWIzYzEwZDM0MQo5ZjlmZTk2MGU5NGY0YmU4ZmIzYzkyMTdkZjg2MmVl
                            |NDdhYzhhNzM0ODhiNjMwOGFlODA0MzUyYTFjMmEKZTFjMjFmZWVjNmNkYTkxYjE5ZjFkN2Q3OTAx
                            |MDQ0NWFmMTExYWVlMWE2NzQwNjFmMTFmZGUwMTZkZTM1CjRkMmMzZmM3NDBjM2I4YmJlYWVhMmE0
                            |YThmNTViYjhjYzJiMGVjODUwMGNiZTdlZjEyYzMzZWE4MjE2Mwo1ZGU3M2E3ZDNhMmM3NDYyMDZh
                            |ODhjMDM4ZjcyZDdhODU1Y2Y0OTAwZTMxNjFkNzE3MTU4NzUyNzhmZTIKNzhkYjk0YjBiMWE2MjE2
                            |YzI1NzQ3YmFhYTc1NTY5YjBiOWRjNGI5ZTExNGE0YzkyMzk4ZjU5ZjI4NmY5Cjc5YzI3NTdhN2Y4
                            |YTk4YjBkZjM2NWE0ZTdmYmFjOGQ4YjY5ZGQwOGI5MTU3ODkyZDg0Y2Q2YTE4NTU5Zgo5NGNjZjBj
                            |NmMzZmRmYTc1MmViMTJiNTQxN2I4YTk0OWU5YzNjZTY5ZmRiZWM3NjJjZDc4MzIxYzI1NjQKYzJl
                            |N2MzODBmYTA1OTU4MTFiZTdmYzUxMTNjZTQ3N2U3NDNlYjIzMzMxNDUxZmUyNjgyNmZiYWYxNTQx
                            |CmMxZjdlYWEwYzljYTBkODE1Zjc0ZjY5NDM5NzRhYzJkZjZkM2JjZmIzM2U1ZjNlYTY2N2Q0MGUy
                            |ZmI4ZQo3YWY0MmI5NDU4MzdlNDc0NTg2NTg0NjA1MmI4MGY2YWY2OGQ5YWM2NDkxNWU2Yzc5Nzg3
                            |NDk3ZjM0NWUKZjUyM2RiYTMzOTZjMjExM2YxZTg5NDg2MTZkYmI5YzQxMTQwMWNmMTliMzZkODhl
                            |MzJjZjVhNjY4M2U0CjY5M2M1NjU3ZDQ4YzhjNGU4ZWUwMDI3OTNhYWY1MjY3NTk3OGQ0Y2JjODdi
                            |MDVhYjY3MjM4N2IzYTdkNwo0Njg0NGFjYzIzNDUzN2RjYzk1NjUyMWM4OTBmMzIzZjRmYzRlM2Vj
                            |ZmYxY2RmYWQzMGVjMzZjMWY0YTcKZTNkMTI5NGU1NjFiZmJkZDVkYWZlNjA1ZjE0M2NkZTRkNGQz
                            |ODU3NGRlMTRiYzFhYmVmMTM1ODY3YzU0CjhiMWFhMWFkZmJmZTdjODBiYmMyNWExNWUzOWYzMGQ1
                            |ODEwNjhmODA0ZDUyMmExNTgzNjU3NTNjOTk3ZQoxNTM3ZmI3MDhjODg0NmRhNTkzOWE4NDUzMDY3
                            |NTAwMmU0M2RlZTgyYmE5OWQ1NmYxMThiMjRkNzRhNjQKNjA3ODU3N2Y4ZDllOWUxODAzZTcyNDg2
                            |NDM0YTdhYTU1NmZiMGZkM2E3MjFkMjJiYTI1YWMyODFlM2FjCjk1ZDg3ZDZjYzliMmU5YTllMzU4
                            |ZDAyNzdjMjVlMWYyNDU4ZTVlMzZjOTIzYjM0OWQyY2Q1ZmM3YzZlMgo1OGFjY2U5NTNkYjg0OWZk
                            |ZDk5MTg1OThlMGRhNGJlODJjNmM1ZjE3MTE5ZTVhOGY2N2MyMGIwYTdjNWIKZGVlNWY3Yzk2MDIy
                            |ZDllMzllYzgwMmI2MWExOWFmNjYzNTYwOTM2OTNkYjQ1MzhiMTQzZWIzOWVhZGRmCjRmNjY1YzBh
                            |MGM1OTI2OGU3YmU3OTRkZDk5ZGRkNTIyYzgwOGE0ZDdjNzdmMWY2NWVkNTg1Zjk5MmY1NgpiNzI4
                            |NTFkNjQ2YWY0NjM4MTMyNzNjMGQxMjYyZGZmM2E0N2ZjMjAwZDc0ZmYyNWQxZTgzYzJiOTA1ZDgK
                            |Njg3ZmU1MzZlYzk0OWVlNDI5YjI2OTlkMjk3Yzg2ZTMwNWViNTBjMTNhYjY1YzI4NWVlZTA2ZTdm
                            |YmM1CjExZTcwMzZlZmIxNGY5M2U5M2Y3NzY2Mzk2NzliMzcxODRjYTZlN2FmMzdlZDU5ZTJkMzcz
                            |NjE1ODJlYwpjNGM1MGY2NGUyYjRlMWZhNWYwYzkyNjE0ZmJmY2ViODlmNmYwZTQxMTdkOTdlNmQ3
                            |NjQ3NzE3ZGNkYTIKMTkzZWFkMmJjNTZkODI0NjdmNjIxZmU4MTk0ZDA3NGE4ZGVjOTA5MjcxNTlk
                            |ZGE2YmE2YzJmZDM4M2QxCjMyOWQ2NDQwMjY2YWNmMzk2ODQwYTVmM2M2YWI5NWNjYzE5ZmYzNDQ2
                            |ZDYyMzgxYzI1YWE5MjIwMDk2Nwo1ODdjM2QyYjZjY2UzYTMxYjg2NWJkYjM1YjJhMTZkMzQ5YWRl
                            |NmFkYTJmOWYyNjE0OGFkNTg3Yzc1OGYKOWE0MDQ4OGZiNWU2NjUzODQ4OGY2YzlhNzU5ZDJlNmJi
                            |ZGQxYTFjZGM5NzkxNTM0M2E3OWQ5ZjQ0Y2RkCmFmMmZkNGU0OWZhOWJhYTJkNDI4MDA0MGU4NGQ2
                            |Njg0Nzk1Zjc3ZDRlYTg3MTE3OWM5MDBjNjRmYTViOAphYjZiZWQyMGM4ZTMyYjhkOTVhYWI0Mzlh
                            |NzlhYTE4ZGJhY2FhZDZlN2JkYzg3ODFlMmNkZjg1OTMzNTQKZmY1ZmUxNTNjODZkMzExMGJkNjhk
                            |ZTk5MWQ1YjIxODQ4ZDI0MjY4YzlmZjc4NDg2ZjNlOWVjYjliOTJkCmQ5MGFkMzhhNTViNDNjNzVi
                            |Zjc0ZTNiODE4ZDlmNTJlOTlkMDY1OGM2NjY1ZDUwMTRiNTE0NmZkM2JmMgo0MDgyYzZkMzNmOThi
                            |MWI2MGJkNzgyY2FkYjlhMTNiNTFlMWQ5ZGZiZTBmN2E2Y2M2MDA2Y2JhYzk1YWIKODFjMTJjYzRh
                            |MDg2OWE2OWZjMmU5NDI0ZmY4NjZkZTI0NGM2YWE4NGQ5OTk0MjBjOWU4YmIyYmNhMGI4CjY3NWNk
                            |ZjYzNzllZjQwZTBhODVlNzRiZTg3YTMyNTcyYWFlYTNhOGUwZDFjNTIyMTVhOWMxMDQ3YTVkYwox
                            |ODhkNjY2ZDdjOGQ2MDRkYmE3Y2I5ZTAxNGU5MDI5YjhkNjc4ZTg4ZDQ5ODIzNDhmMmExZTA5ODZl
                            |NjYKN2M4NTdmNWRjNjc5NDlhYWVkMzg5MTc0M2YzZjk5ZDI1NDY4ZTBiNmY4MDMyMTgzN2JlN2Mx
                            |YTI0NDFkCjg4NjhlNWU0ZjZkZmQzMmJmYTlmZjA1ZjQyZjlhZmRiODk4ZmFkOWYzNTU1MDkwZGFh
                            |ZWQ5ZTEyMmViNAo3NjMyZWExMWM2ZDhmOTcyYzlmMjRlODRkMTYzMGEzYjg2NDBkMTk4NzVjNjZj
                            |N2U4ZWY3Y2ExY2VhNmEKYTU5NTZlNmIxZTI1M2ZmNjE4YTg0MGIwNDZlOGIzNTRmMjhhYWZkNzYy
                            |Yzk4YjA3ZjZhMzQzNjQxOGM2CjBlMGU0NGE5ODU5ZDY5YjIyN2YzYTcxZDQzNmNlOTJkMjY2ZjBk
                            |NzFmMmExMTg1ZjA2YjJhMzBmZTI0Ngo2OTdmZjlhMzEyZWI0Njk5MDVmZDM2N2IzMTJjNDM0NTgx
                            |YWEwYjQxZWMyZmIwNTQ2Mzg1MzQzMmExMGUKMzZjYzcwOTI5Yjc3Yzg3MjY0NjI4MzA5OWNjNjMy
                            |OGUwN2NmZWZhN2RhZDAxNmIwZTI5NjMyMmYyNjFhCmYyNzA1ODkwYThiMjkzYmY2YWRhNzdmZTk1
                            |Mzc4MmFlNTRmYjBlMjhmMjY0ODkxODdjYjk4YjI4ZWUxMQo1MzIxYzc3YTNhOTJjMTg1MjVjZTFm
                            |MzRlNTAzY2FmZDhjOTJjYTZiYWJhMzRhMjAxZjMxN2NkNDI4ODcKYjllMDIzYjY2MDYyMzRlNTJk
                            |ZjIwMmU1Yjg1ODZmMmI0ODUxMTg2NmZmMmRjNTBjODYxMTI2YmFkYTdmCjVjNmE5NzMzODcwNjdl
                            |MDEzNDBmN2I2ZTE3OTBkNDYwNzlmMjEyYmYyZTQ5MGMyMDM2MWE0NzBiZjUwNAo1ZDVmODI4YzY5
                            |MDNiYjgxNzJkZWUyZWFhNjExZjM2OWFjYTkwNWQ4MWY2ZTVjNGE3YjIxNzNlNjk0NDIKN2M4ZjA4
                            |NjIwYmE3YzdhZDc0N2I3YmY2OGYzNGRiNjNhYmYzMTcyMzdiZjQ0YzY1MjBiN2E0Njc0MjhiCjg3
                            |OGU2MGFiNzI5MGY5NGU3MWEyNmQ4NTcwMmZjODM1ODM1ZWNjNzA2MTg4Njc1OGM2ZTQ5YTExM2I3
                            |YQo0MWUxNmE0ZmZiOWVmYjRkNjQxOTQ2OTUyOWU0NTFhZTE5M2ZjYjc1MTI0NTIwNzNkOWQ3Mjg5
                            |OGJhNTkKOTIzZjFkMWFkZTdlNzZmNmZlYTI1YTY4MDQ1NGQ3MTAwNGY5YjNjMjc3ZDEwMWY4MDEx
                            |MWVlOTQ3MTQzCmU4ZmNmMmVkZjhlNjA5MzA2ZTJiMzMxNzgzZWZlZDY1ZWUyZTgxYTIxODFhMzEz
                            |M2UzMDhkNGVhNDE0MgpkZmI2YmMxZGVhYjM3ZDI4OGY3YTg2YTRlZTM0YzdjZTRmOThlM2E4ZjM1
                            |MTkzNmJiNWQwM2E1MzY3OWEKMzNjMzgxYmM4NzU0M2EwMzdlMjEzNjY5YjQzZWRiMzkxNzVlMTdm
                            |OWEwYmUyMzU4ZDUyYTc4YjI5Yzk2CjkwOTJiM2NkMGY2OTY5ZTc5MmJmNjBjOWVhN2EyNGM2Zjk5
                            |N2MzYWE1ZTgzNDY5YWVkNWE2ZTMyNWE3YwozNGQwMzkxOGYxMTA2Yzc3MzI3Y2E1YzNjNDFiNzM3
                            |NmNkYmNiZjZmZTcwNjhmMjBlNDQ3OWI1ZWE3YzEKN2M0ZDg5NDQ4MzQwODgzMmFlZGIxZDY2ZTY0
                            |YThiYTRhMzMxYTU1NGNkNGVjNWM1Y2ZmZDNmYjgwN2RjCjE3ODQ5ZTMyOTExYTFhMTM1Y2M5YWZk
                            |Mzk1ZDhmNzU4ZWNlYWE1NzkyZTVkMjgzYmViYTNiYWU4N2JiZQpjZjczNWM1ZTY0YjFkYjc2MTQw
                            |NzZlOTU4OWU3YmVlMmY2MmVhMDVmMDVmZmFhMjEwMDQ0MzBjYzg1NWQKNWE3YTJhYTU3NWVmMDIw
                            |YWFkZjUzNjQ0NzRlNzhlM2Q3MzQ4MDEzM2M4ODE5NzY5ZTQ5ZjE0OTVjMTM3CjVlZmI0Nzc5Yzg4
                            |ZDMxNmFiMjI0MTZhZGM5MTAzZGY3MWMyYTY3NTM0MDE4MjRkYTI1YmM0MzViOTA4NAoyOTI5MDA0
                            |MGIzNmNjZGU0ZjgxMjI3NzRlMjk3MmQzYmMxZmVlNGU5MzhhMDY1NDhkYTJiNGZhYzI1MWMKODE2
                            |Yzc3NWMzNDY0YTY4NDM1OWVhMjRlZmJjYTk1YzBjYWJiMWQ4NzE3ZmQyOTUyZGQ0ZDY1NTgzZDRh
                            |CmU4ZDM1MDdiMmNjNzYyMWJiNjBjNGM2NjY1NjJhYTA4ZDk0ZWVkYmYyY2VlOTg1NDEzNGE0YzY4
                            |NTBhYQo1MmM1ZjZmYjU5MDYyNGU5ODViM2RlOTk0MDUxZDViZTUxM2Q4ODUxOTQzZWZmYzllZjBk
                            |MTY4ZDgzZmUKYjQ4M2JlOGU2ZmM1ZjE5ZjFjMDBiYjAwZDc2OTNlZjg1NTljMmNkY2VmZmUzODEx
                            |NGI5ZmJhZjNlZDlkCjVlYzAyNmUyMDZhNTI4NjdlZTcxODkxNTMwMTQ5ZGM4NGU1ZDgyY2I1NDgy
                            |YTk1ZjViZmQzMzA3ODhmMQo0NWE5NzgzZTYxNmVhYTcyNzMyODYzNmNjMzNiNjhjOTczODFmMDNm
                            |NWM2YmNlYjUxMzIyNTA1OWM3ZmEKYjYxYzM4NWQ1YjE4NjYwNDVlYzQwOTU0ZDhiOWI2MzViZTRh
                            |Yjk0MzM0ZGI1Y2YzZjg2YjczODg3ZDFhCmE1Y2U0MDZkOTNmMzlkYjg4ODEwNjNhMWY0ZjlhNGVk
                            |ZDdhMjVjYzFhOGJiY2UxNTZkMzFlOGFmNzRhMQo3YjM3NTE2ODY3MjRhMTJlZDZkYWI0YTUzMzNh
                            |ZGE0ZGNkNDU5NjZlNTA1ODI1ZGFhZGU0ZmVlODUyYjEKMjBjOTU0Yjg3M2E1MmZhYTg3ZTZhOWQ3
                            |Y2ViMDMzNzA0OTVhODAwMTliNTY5MDA4YTllYWIyNTllYzZhCjBlNDg3YTNhNzFkNDlmMzMyNTVj
                            |NTRjNTk5MGY1NjczMjAwZjkzMDBhOTE1NGEzYWQwNGQwOGNmM2Q4NAozNjdhZTg1MWI0YzI4OWFj
                            |NzliOTY4ZDdiZGIzMTU5OGU0MTRkMjRhYzI0MTIxYjY5M2Q2OGNlM2Q2MDcKYjlhMzkxNzU1ZGQ2
                            |MGQ2ZWJhYjYwMTgwMzBiYjhlNTRhN2JiMDAxMjY1ZmM4Mzc0YThmZmZkNjNkNThjCmZiOTY1NTQ3
                            |ZmMwOTJlNDBlZjVhZDY3Yzk1ZWMzOTUwMTE2ZWE2MzIxYTY3NDEzY2ZiYmIwODRhZjg4NwplNmI2
                            |NzUwNjcxOTY1NjVkZGZhNzlmYjE2NmM5NWNhODJiN2U3MTc4MTU1ZjA4MjkzZTlhOWM0NWQ1OTMK
                            |OTRkNzA1Yjc3ZDA1NWE0YjkxMDVmNGFkMDg0Y2NhZGM1OTA0NTBmNzJmNjI2MjhmNWJkNzdiNWRi
                            |OTMzCmE2ZjI3MzM0OTQxY2VhZWI4MWQ5Njk3Y2M4ZDJlMWQ5YTM2N2Y4ODJhZGQ1YjJkNzAxNDkw
                            |MDIyYjhlYQo2ODliZDliZTVmN2M5ZmU0ZmZiNjU4ZWIwNDNmZmIxMjZhODQ0YmQzODkwMzE3ZjA0
                            |ZmU4NmRhMDRkZTQKYjY3OTk0MDc3NWZmMTk3ZGU0OTU5ODhiZGVkOWI0ODJhYjE3Mjg2ZWU0OThl
                            |Yjk0ZWM5MDA5NjNjYWE5CmQwMjI3MjljZDE1MzdmOGY5OTViOTBlNDg4Y2E5MjU2MmM2MDlmNzZh
                            |ZmEwOWI1MGI5MDkzMGRhY2U5MApkNTU5ZThmM2I1YjhiN2EyOGNhZTcyNTgzZmIxMTBlNDM0ZjM0
                            |NjRjOTEzNWU3YTJmZmZjZDVlMGZkYWUKMWZmN2FlOTE4NjY3ZTljNjQ0OGQ5NmQxNjc0MmIyYzA1
                            |OWRiMWI1ODg5MTJkZWIwYzE0MmUxNWViN2MxCmIxNTYzMzdjY2QxODM2MGY4OTJjOWY2MDRlMzBk
                            |NWVkNzcyMDdhNjhhNjU3NGIyNDgxYTliYmJhNGI2MQo4MTI2YTQyYjFhMWI2YzM4ZmRlMTE0ZmQx
                            |MDBkNWIzOTgzY2UxMmMzMGFiMTM3YThlN2QwNzdiODQ4NjYKMzU5ZDdkM2I1ZmFjNTcxYTVjNDIw
                            |ZmMxYWRkYWVjZWIyMDY4MTg2ZjIyMjE5YTJhOGQ3NjA0ODFhZjNiCmUwYWIwZWMxNWQwYzU0MjBj
                            |NWFhNjRhNTFjODNhN2NkNzMwZTliZTkzMGU5MjkxNDkwYWVkNDBmOGIxMgo0MTVkY2MxYmRlYjk0
                            |Nzc2MzBjNTUxNWQ1NDdmYzFiMmVmYmExOWJmOTVmNzM3MGY2MTYxNzVlYTc4ZWIKODFiMTZjYmMw
                            |MGI3YThjNzIyZmNjZmZjNGZkNzZiMGM3OTAzNTVhMDFmZjUyZDE0YzM3MjBiMTk1NTA2CmQzNTFm
                            |ZWRjNTdiNzFiMzIyYTNlMDUzZjg0NmExNWEzZGY4Y2RlMTczOTgxY2E0Y2U2OTIyOGIxODRiNQo5
                            |ZTY1N2ZjMWY2ZWJhNDg3M2FhMzEwMTc4MjQ3NjQ1MjNiMWQ2ZjU5NDc1MDE3ZDVhZWI2M2Y2ODhm
                            |ZTEKYzlkOThlOWM2NTIyOWE3MWEyOGU1ZWI3MmM2M2IxMmZkNmUyNTVlZjE5ZDgzMWNkZmQzMWMx
                            |OTc2MzQ2CmExNzQ4NmFlOWU0YzhmOWZhNjFjMDQ5NDA4YmEzOWI2MmRhNTRkNDg4YTk5ODZlMWMz
                            |YjdlM2E1YjA3ZApkOGIyNTVlNjg2NjBjMzdmMWRiMTczYTg3NDA0YWVkZjE0ODE1NGQzZGRkNTJi
                            |NTZmNjMxNGExNTk4OWUKZDQwZjI5Mjg0OTNhMzRmOWFkZmFhZGVlNDc3NWQ3YmY3OGYwYTUyNmZl
                            |YjFjMzFmYzYyNmY1MjIzYzgxCjE3NDA2MDNkOTU4ZGMxNjVhOWFjNDc2YmM1YTRlNjNhNTZjY2M5
                            |MzE1NGYyMWIzNTRlMTk1NGU5YmRjMgo3YTU2ZjM2YTFjNTkzZTEwOWJjNjEzYjI1YjMzMGQ3MGFh
                            |NTY1M2NhYjRjMTBmODNjOTcxMDJmYmQ4MDgKZWFiYzE2Zjc1MmVkZGYxY2E2M2EwZmE5MDJiY2U2
                            |MDQ3ZWQzNDEwMjZjYjIwNDUwOWM2OWZhNDgzMzgwCjc1NGE3MDhmNTZjMmRiNzEyY2M5NDA5M2Fi
                            |MTFjZTk3ZDVjODdlYmQ3ODE2MmI1NjViNzAyNmEzM2M0NgpmZjNhYmJlOGZmYmE1NGRjOGY4YmE0
                            |NWNjM2QzZDE3NjgzNTU3MmE4MTdiNDU4MzY1MmRhMWMwZmFkOTAKOGEyNjliNmU2ZmY5MjhmMjA2
                            |YTRjNjE5N2VjMGE1OWFiMzY3NzlmYWVjNzNhOTFjMjc3ZDA1NjMwMDAwCjAwMDAwMDAwMDAwMDAw
                            |MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMjc3ZDA1NjNlYzczYTkxYwpiMzY3NzlmYTdl
                            |YzBhNTlhMDZhNGM2MTk2ZmY5MjhmMjhhMjY5YjZlMWMwZmFkOTA1ODM2NTJkYTcyYTgKMTdiNGQx
                            |NzY4MzU1YTQ1Y2MzZDM1NGRjOGY4YmJiZThmZmJhM2M0NmZmM2E1YjcwMjZhMzc4MTYyYjU2CmQ1
                            |Yzg3ZWJkMDAwMDAwMDEwMDAwMDAwMAo=
                            |]
            Player ->   [str|NDZiZjcyOTU0ZWUxMDA3MTUxMTVkM2E5YTIzOTQwMWQ4MzI4NzY0YzYwOGUzYzAxMTJiOWJlN2Rj
                            |MGU5CjRjNjA1OTIwNGRjZDI2N2M4OWMwNGE5ZTMzNDNlYTk5YzJlMWJlNGUwMTgwOTZjNzRlNmMw
                            |ZTgyOWVjOAo1NmVhNmM5ZjNiZmUyMTNhMTRhMTcxM2RjNDQ2MGJjOTg1NGYzODNiNDU4YzQ4YjEz
                            |ZTAwZmMxOWUxMGEKZjZkMDhiMWEyZWMzYWU0YzZkYTY0Y2NiMGZiMDA1MmJmYTMyMGQyMWRmZjBl
                            |YzY4ZDhjYTkwNTU2NGI4CmY0OTUxODVhMTI2ODVjZDNiY2ZlNjkyODQyYmYyNjIwOTFlZjgyNjk1
                            |ZmE0MTI1NzQ2ZjhkNjI0ZDUzNgpiZmY3OTI4ZmI5N2IxYWIwMTc0NTYxYTAzOTQ0N2UyZDFiZmNi
                            |NjUxMmM2MDg4MmQzN2JmNzkxZDc3ODAKM2M0ZGY4OTQ2N2ZkYzFhMWQxNWQwYmFlZDQ1MjczZjRl
                            |YmMyMWQ4YTE5ZjlhYWExMjA5MGFmODc4NmM3CjE4MjQ1YmMyNjNiMjM0ODJlZWYzMDkyNTZhZTkz
                            |NTBhNmNmMGI1MGYyN2RlMTAxZDc5Y2U1YTM2ZjY2MAo5Njc3MzMxODc5OWJjNDhlODk5MGZlNGI0
                            |ZWYxOGJhNzBlYTc4NWNhYTc5NzE3ZjYyZTE0ODNkMDlkNWQKMTE5MDJmMTFmZjNkZDBiOGMzODhl
                            |MWY2MzllMWY5NTcxZWIzZDUyYTljYTM3ZGZmMTFlMjFhNTA5ZDRmCjY1ZDQ5OTNkMTI1ZDM2NWYw
                            |ZjVjMTc3OTA0NjgxNDM2ZTk5ZTg5YTM3YjdjOTVlZDI0OTE0ZTJlMDRlYwpkNjkxYjA2NGI0MmFl
                            |MGEwZTY0NThlNzhhZWY5MTg5OTM4ZjlhNjIzZDI5ZDQ2ODQ2YTI0YjVmODc2ZTYKN2ZkYTQ5OTVm
                            |NmE3ZDQzYjVjMDMzM2NkNGNhZjU2YTVmMDQzZDA2YWRiNjBjYjhlNWFiMGExMTI4MGY0CjllM2I0
                            |MjU3MDIxMWFkMWZkZjlmZjVkZWQyMGNkNjQ5ZDM0YTFmNWQxOTU3ODVkMzFkMjQ1YTBmOGI3NAo0
                            |NzE5YmFiZTFiZWI0M2MxYjFiYWZkYTNiZjU1Yjk2ZTgzYTY0YzZhMWU5MTAzYmFkNTljN2NjZGMy
                            |NjkKZDYyZWUyZTc4NDNjMzc5YjZmYmEyZDUwNzhiNmY5NjkzM2QwMjFiZTczNTczOGI5ZGIwMjNh
                            |YWUwMWU0CmNmYjk3YmFkNTc0NTExYmE2MTM0NzYyMDliZDMzODRhMTcxMjM4NDEwMTBiNzRjYzQz
                            |YjEyNGUzMGQ4OQpkMWU5ZGM4OTcxN2Q0NjFjZDExYTcxZmYxMTA5YTgyYjBkY2MyYzU4ZDQxYTcw
                            |OTE2MDRlNmM0N2RmNDMKMmRkMjkxYjM1NjBjYWVmOGRhZjgxOTMyNTk3MTYzMDkyMzg4M2EzYjU3
                            |NTAyMjE5ZGFmZDMwZTI3ODNjCmMwOTFlOTM0Y2U3MDFhODNjMjcxZWMyMDMwNzljYTJhZjAzZTA5
                            |YWJlMTg0ZmNhODkwNzdiYzM5NzQ1Mgo2YTI3ZjAxNWQxYmNkMmM3YWIwYzQ5NTc5OWNhNGM1MjA2
                            |NWUzN2ExZjdlMWQ2ZDYwYzYwZTcxNWIwYzcKZDMxYWJhNjVjZjU1NzFlOWY4YzM5YWNhZmRhMjZi
                            |MDQyMTI0MTQzNzQ3MGE3MWU0MjQ5MzMxOTY5NDRjCmI3YWEzYWU3ZDZmNGY3MDEwOGIyMjI0ZDA1
                            |OTNiNmMwMzY1ZTY1YmIwMDczZTE5NmU4Njg5ODVhZTE5MAo1MjQ4MzEwZTJjZDQ4ZmVjZjZlNjM5
                            |ODJmNmU5NGEwZDU0MzEwNjM4Nzk3ZWYxNGU5MDQxMjA3ODkzOTUKNDYxMDNlZmVlNTFiNmQwYWU1
                            |NzFiZDVkMTk0YzFlZWM3ZTNlZmUzMjUxYWJiMDQ5NjYyNDM2MDkyZGQ4CmMxNTA2M2ZmYjFjNjg4
                            |OWZmNmU2NDM3ZTM0ZmY5YWY1ZGQ3YzNiYWFiZTBmOTc0ODBkOWFjMDAwZTg2Nwo5ZmY1MTI0OWQ2
                            |NDIyMDViM2Y5ZjE0ZWM3OWI0NTI5ODM4MTg5Mjc1NWZhZmRkNDk4ODQzNjU1YjNmMzEKZDc3ODgw
                            |MGI5MmQ4MDViZmVjMWViYjA4MjU5MTU3ZDc3NjU5MWFmNmQ3MjJmMTE0Nzg1MWMxZmJhN2I0CjY2
                            |NTdjOWQ5YTkxMGNkYjQ3MTk4ODZlNzNhZjM3MzIzMGMwNGJiMzgyMDVhNGU2Y2U0Y2VkOTIxZWMx
                            |ZQpiN2RmYTJkOTY4NTdjMDdjYmNjMDI0ZWUxYTk2YTUyZTY2NjBjNTUyMmI1NmQ4MjljZGJlMDE5
                            |MTkwNTgKMzVmY2MyNDJlMDFjZjBmZjUwZDU5YWY0NjQ2YWUwYTAzZDNjYzQ1OTYzODliMzZlNTI3
                            |MTVjZmNjNGU2CjljNzBkNjYxZjdjYzZiNzNlYzEwN2EwMzBjM2NmNjgyNDNiMDY1OWQyYjMwM2Y3
                            |OTEyNjdiYWI2ODQ4MAphN2JmNmQ0MTFmZDk5MTgxNzA4YWNhNTFiNjQwOTMwYWNkM2Q3YTEzYzU5
                            |YjEyZmFiN2ZjOTEwNjJhMmEKMWExZTMyNzE0ODU3NTc4ODdlNDIxYTdmZGY3MjkxMmYxMDFhNjYw
                            |NDJlMGQzNDAzN2E3YmVjZGI2YWE0CmM3ZmRlZmZlYjMwYzdlZDY5ZWY3MTk2MGM2OWM3ODRmYjRj
                            |NWNiNWQ2ZTcwYTdlMjJjN2Q4MWIyYTdlNgpjZjY2MDUxYzkxNDRhYzI5OTAwYmVmNTY4ZTI2MjU4
                            |MTg0NmZjMjE2MWMzZTE1MGJkYWExZjczODE4MzUKYWZmYmQ4ODkxZGIxODRiMTE5MTEwMjIzYzIz
                            |NzEwNzlmZDliODMyNTA4MmVkNjlkNDg2OGEyODEwYTZmCjczYjFmZmFkZjQzODg0NGNkNjdiMzIx
                            |MjQwNGZhZDU1ZmZjYjRhYjY1Y2YzMzAyNGJkZGQ5NGRhOWQ5ZAplYzM3NzM3M2NmZmE0MGNiZTQ0
                            |MzYxODIyYzk5NDRmMDhmMDU5MjQyZTQyNzVkMzIyNTY1NDM2YjNhN2UKY2UyMDNmNDJmMjRhZWFl
                            |OWUzOTFmZTg0YWQwM2Y3MzMxZjc0MDgxYTFiYWVkOTJkM2VhNGZiYWIwOTRlCjI4MzAwNGU0YmFm
                            |Nzg3MDgwMjUwZGI1MDRiYTJmMDY2YzExNjgwY2JjMTcwZGU0NzRmNWU5NjZhZDUxYQo0MGI2ZDNm
                            |OTVkOGRiMzAzYjdmZmU5ZmQwNzVmMGIyMWUyMmE5NDYyOTBlODg5MzZkOWM0MDgwNzRmNzYKOWRi
                            |YjZhNWI5YzAxZmQwNTY5MDgzNzM2N2I0YTQ5NTQ5ZWMyMzQzNzQ2OTE3YjlmOTdmMzgwZDY5YzNl
                            |CmRiZWVjMzI2ZTU0YjkyNTU3NDg2MGY0YWNhMDYxYzU1YzUyNWM1NmU1MzFlODE0ODg5ZDYwNWI4
                            |MDViMApkNmNkZmZjMjkwMmNhZjQ1ZWE1OGYzMDRlMmQxYjE3NTM4M2Y2ZTFkYTMyODJhNTczNTYw
                            |NDFlYjkwYWMKZDA3MGI4M2NiYjEyYmZjOWFkZTI4NzU3NjZmM2ZkODJiYzZlY2MyYzBlOTlhYzNh
                            |ZTExNTA3MjU1ZDhkCmVkYjA0NTEzNmIwODIzZDg0ZmQ4YjQ5OTkwMzM0YzE5NGNjZjRkMTI1ZDkx
                            |MGEzOGFhYjM0MzU0ZjQ2ZApmNTc1OGZkZmQzZTA0MjgxODMyYmI3YTY3ZGI2NjNkNGZkM2ZjYjk0
                            |MWNhNjgzOGU4YmQ2YzViOGVmMTgKYWRkOWZkZjhkZmM4MTExN2MyNWUxZjBkNTc3OGNhMjE1YWI4
                            |MDUzMTFlNTg5YzY1ZTIzMzM1ZjRjZmJlCmRiM2I0MDg2ZDYwMmQ2NGVhOWJiZGFmNTA4MjQzNjgy
                            |NzZhMWNkYzZlZTQ5ZjYzYzA2ODU3M2FjNTRjMQpiNmE3NDAwYTdhYjU1YzJkMmQ0ODZkMTIwODhl
                            |NWRkYTRiODM4Mjk2MDVkNTM0ZWZmYzRkYTAwNDUwNTMKZDVlNDc0YjdhMDE1NTI3Y2Y0N2Q1MjYx
                            |NGI0NDE0ZDcwMDhkNTg2NzJkNDhmYmJhOTE3YzllNzY4YjgzCjBhNTFiNjBmYWMwYzcyNzg0MzRl
                            |ZGEyY2ZjYTA4YzI0OWY5MWUzYmY1Mzg5ZTM0NTcyODVkZjdjMDNjNgozOTg5NTgzMDk1ODEzZTRm
                            |YmE4MjYxZjllMmE5MWY1MTRmZGNlYmY5YzQ5ZDE4MzAyYzdlY2I1OTAzODEKNDAwMzk2ZTM5NDMy
                            |YTU0MjViODM3MTk3NWQ3YjgyYjI1M2U3OWZjZjFmOWFiY2ZkZGE3ZGI3MmM1YWYzCmYzYjFiMTEz
                            |MDRiZjI1MTZkZjhmZWVhYTY5Mzc2MWUxMmJhYWU1Nzc5YTAxNmIzMjJhN2Q0MmFlNWRmZQplZDY2
                            |ODQxYmM2NDliNzBkNDVlNDVhZjM2YzdlNjYxYTk3MDI0MGVmODNjMjE3NWY3NGMxM2I2ZWJkZGQK
                            |YjkxMzFjY2E0NjNhMThiYWE2MDg1YjAzZmJhZGNkNzRmNjU4M2JkNTJmNDMyOGFmZTU0ODIzNjIy
                            |NzU5CjFjZjc4ODNlMzYxYjg5MjgwZWRiZjRjZjljMGFhMmMyOTVjZmY5ZDdhZTA0ZjcwNzcyMjc0
                            |NTE5MjdkMQpiMjU0NWE0YjJhZjg0NzE3ZjQ3ZDdhOTBmMDlkMTFkMDI5NDlkNzdmYzUyYzJhNzRm
                            |ODAwOWY4Mjc2ZWIKYjYyYzA2NjBiNWVmZTNmZDc3ZDYyM2Q0YWY2YjdkNWIwOTFlYWFkMTQ5ZmRl
                            |MGI1ZTYzNzBmMmRjNDlhCjA3ZjI5OGNkMGZiMDE1ZjY2OGIxMGY1Y2E2NGZkMDM4MTE4ZTg0ZGVi
                            |MzJhMzNlM2JjOTQzNGY4NzI3NAoxMGRlZmY1MWJiNGNiNzFlOGNmYTY3NTIxYmFlM2UzNjlhODY4
                            |YThjMzljY2E1NGIzMGVmM2VmMWZkZDQKYjhlYzhmZjE3ZmVhMmU2YzYwMjlkN2E4MmVlMGI4MDBj
                            |MGEwZTczOTc5NWQyNTk1YjI0OGRhYjBmY2ZjCmVmMDgzNTkwOGM1YTg3NjlmNjgwNDA5NmUyNzRl
                            |Njg3NGM3NjVlZGIyYjU5YTFmNjFkYTM1MzlhMThmZQo0OGIxNWU4MTk5ZjA0OTM4MDYwYTA0OGM2
                            |YjVkY2U2ZmNkN2U0ZGJkOThjNzgyNDJiZGQ1NjM2YzQ1ODAKNTk2YTZlODNmOTY1NTVlOTRmMjE4
                            |M2E1ODU5NmM5NmJhYTVhZTMzM2U5ZDY4MTM1NDUzYWMxOTZkZWRkCmZjMDEzNTg2NTA2ZmEyZDM1
                            |ZDY2OWE5YmM1N2ZhNDY1YTM2Y2EzOTI2Yzk2M2UyNDU3ZmY0YTlmNjFlMAo4ZDU0ZDJjYjhiNmUy
                            |OTk4ZmJjNDQzODdjZDU5ZGQ5YmZjZDgxYWJmNjY0MzA2NmJiNGJiOGQ0OGUxZTcKNzRkYzNhMTg3
                            |YmYwZjRjZmI2NzllZTZlMTJiMDgwNWUxYjAyYjM1OTBhOTQzMGM4ZjY4MDlkNzFhYmNhCjQ2OTk3
                            |OTczMWNlZDk3YWE1ZWY4YzViN2ZiMTg2NmU1MGQ0ZTA3ODIyMDQ3ZGMxNGYxYmNjMjFiMjJhOQo4
                            |YjdkMmQ3MjAyMTBhOTkwN2ExODUzYjBmMTA4YjVjM2Y4ODRmYzQ3ODZhNGNhYjEwODZmOGQ1NmQ0
                            |NTAKNTkyMzA4YzAyZTk3MWY1OWYxMjlhNTlkZGVmMmI0YzM3YzAyYTUwZmVlNmVjM2E2NzViODE5
                            |ZGE0Mzc5CjIwYmVmMjAyYzkzOGU5MDE2MGMyOWE5YTYwOGUxOWUwOGJjZWU1OWUwNmZkNTBiZDg5
                            |ZWFmN2ZhNmNiNQpmNmZhNzc5Y2IxNmU5ZjA0ZWFkMTJkMWRmNzIzY2RlNWQ5OWEwYjBhYzY0YjBl
                            |ZjY4ZjI0NDdjNmMyZmIKMmU4NWYwODUxZTM4ZGYwOWUyMmEwZGVlZTVjOWRiNTY2NzY0NmQ5ODRi
                            |ZmVkYTk2MGIxYTcyMmVlNTExCjY5ZmY1ZTUyMzBlOGY0ZTEzNDBjNjlhOGJjMmUzNzBmMWJhNjc2
                            |OTYyMGZkM2RkNDNiMWUwNTliNWYzMQo2MGYzYWFmMGZiMDVjNzAwZTIwMmFjMzQ3M2UxZTY0NWVi
                            |Y2Y5NzZlYTY2MzNhOTg3YTgwY2E5YmQ1NWUKOTYyMGVlYTczODRlZmVjZDVkOTQzZGE3ZjRlZDIy
                            |ZjY1NDMyODlmMzNjMDE2NzE0YjhmOTEyMjFhYjkwCjc3YzNmZTEwYjg5YjZiODM0NGU5NGJjMTVl
                            |NDQ0ZjE2YmFkMzM5YmRlNmJmODc1MjA1ZDFhMjc1ZTE1MgplOGZiMmRhNjhmZGFlM2E4ZGRjODQ2
                            |ZjdhZTk2Y2FhMmU5NGExMTAxNThiY2YxYWUyY2JjZDE1YzcxOTEKNjc5OTE0ZmQ5OWNmNWRmZGQ1
                            |NWIyNDBlNjcyZDc5YjY1MTE3MGVkNTRlM2Y5MjBlZjkxNzlmMWZiMGJjCjlmMmNiYmQxOGViN2Nh
                            |NTI2OWY0YmQxODJlMGQ1MTg1ZjIzYWQ2OWZmNGM2MjM3NGM1Yjk2ZmJkYTVhZQpiZWRmNzJmZWQ2
                            |MWE1NmJjMjlkMzRkNzI3MjY0MTFhMjUzNjY4NGVlZTY5MjBkNGQzNThkMDI0YWE3NzIKOWMxYjVh
                            |ZTk5ZWQyYTU4YmFiNGMzYzNlODRlYmRjNWU3MWQ1YjNkMjUzNTc5ZTgyMWJmOTdhN2NhYzE2CjA3
                            |Yjg2M2Y0MjQwYTM2NjliNDQ1ZGIyYjFmYTc5NTNmMzQzMWUwMWY4ODY4MjVmMzRlNTk5ZDVmNTky
                            |ZgpjMmJhMTZiZjYxMDExODg1NDBkY2ViZmNkYjZmOTUwMzRmODI4ODg2MDhhMzRkMDE0YmRjYzc5
                            |MzRjYTgKYmYxMWFkZGIwYzkyMzYwNGJmMzRhMGE5ZDE3YjBhMDVlM2JjMGRkYzA3NjhmNjVkMjlm
                            |NDlmYTQyMjJjCjU4NGVmYjVkMmVkYzYyYjBjMDdkMTdhYWU3MWZkZWI3ZmYwODFmOTc2MGI3M2Vj
                            |NzU5MTMwMGQ5OGYxOQo0NmJjZmM1ZWI2M2IwMDdhNWMyOTZjMzM0OTZiYjdjZTlkZjlhMjAzODFi
                            |ODc2NGRjNmU0NGYyNWZjMTYKMmU1YjUwODA4NDJjNGFmYWMzODBlNGI2Y2VlYzk0NmI0OGY5NzNl
                            |MWNjNzE4MmFiNjA4Y2U4NWI3ODk3CjU2MjUyYTllYjExNjlmYTg0YWRmNDNhNGIzOTRjZWQwOTY2
                            |OTI1YWMwN2JmNjVkZTkwZTFiMmFlNWJlMgpkNGU1YmYwNzU0NjdhZWYyNjc4YmUyYmZkOWJhY2U0
                            |YmFhZTc3MTY5MjIxMzlkMjYxYWM0OTZiMTU3NTMKNDk0ZWEwOGE1N2E1ZjhmNWIyODU1M2I2OTQ4
                            |NDhjNWQzMzE5MTg4Yzk4ZDdjZjI5ZmJlYTAxNTdmNWE3CjRkODJkNjhjOGQ0NGE4ODYzOGI0Yjc3
                            |ZDM2YTRhZTIwOTliNGFhOTk1MWM1NzIyZWEwNjgyZDAyOWU4Mwo4ZjY5NWMyOTE2NTk1ODFjYWJj
                            |ZjljYjAxZmRlMmI3NWZkMmQ2NTllYWQ4Y2RkM2E3OGNiZTg3M2VkOWUKZDdjNGI3NTI3M2UzNTkx
                            |ODQ2MGFiZjY0NzRiN2MzMDU5Njg0MDg2MWY5YjA2MDRiMmY5NDE4MDVmMjYwCjgwYzI5ZmIzOGIw
                            |Yjk5NDRjZGYxOTUyOWVjNmNjYTVkYjJiMTM5M2FlMmY0ZjI2MjZkZjRmN2NjNTBhNQo2MjcyODU5
                            |NjQyMGY0ZmNiMWJiODljMTQwMDJkNjJjNjBmMDExYWJjZTM5NDM5YWMxMDkwYThlZDliZTgKZjNk
                            |ODg1NWMzZGNjYzdhMDg1Zjk2NWFlYmU2N2M2ODRiOTdhNzIzYWUwZjQ0NjRjNGZkZTJlYzRmYjcw
                            |CjYwMmNjNTRmMWRkN2UyMmVlOTI4NDJjOWYzYmZlNmI0ZGJhOTQ5ZDkxYzMwOGVmYjZhMzVmMGI4
                            |ZWM0NQo0OWRkZTQzZTY3MzQ5MzRiMzZkM2JkYjU2NDVlYzM0YTFjYTc5ZmQ4MzkyYzc4OTVkNGYx
                            |OGI4ZjgyYmUKZjc0NzE4MmZmYzg4MzU2ZGI2YmNmNTQ3Mjk2NTY3ZmY1YTU0ZTNmMjllZDAwODQ0
                            |YWQwZmMyNmY5ZTk4CjlmYjM1MjZmMmY4YWIzN2Q5Njg3OTc5NzdlYmM4Y2QzMTAyMmU5ZGUzNWY4
                            |MjVjNmE4NTFjMGIyZDgyYwo3YTg3NWFlM2IxNjE4NjlkMTM2ODNkMjA4MmMxNTZlY2U4MmM2NGNk
                            |YjZlZmY0M2YyM2ZkYzlkYjNmMjgKNzFjMzE4YjFmM2VkOGJhNzkyOGQ5OGRkNGIwN2NlYmE3ZWE3
                            |OWU0M2EzYjgxMmNmOTA3YjExNDk2MmI1CjQyMGEwOWM3ZTI2MTI3NGE0ZGYwMWY1MmQwZDc3Mjhk
                            |YzFjNTEzZTk3YTExNDU5ZTA4YmFmMjBjNDFiNwpjNmFlNWJjNDEyYThiNzYzMDdhYTAyNmJmZDFm
                            |MjZjM2ZkMWZmZTE0Zjg4YjM4ZjkzNDNlNmIwMjI2ZjcKOGVjYzJlZTFlMzAzYzRlZmRmZmZmMTdk
                            |YTBkNGZhNGZhMTQ0MWQ4MWU5NmUzMThjNjA1ZjI1YzZmOTk5CjgzNmE2MzA4NWVlOGE4ZGViNWJm
                            |NGY5NDg3YjMxYmQyOGFjZTZkNjk0MDIxYTQwZmM0YjRhYzRjNmI2MQo5MDc0ZDEzNzUwZGIzNGFh
                            |ODQ1MTQzOWJjNDlhZTkyYWZmMmVkOTBjMWY0MDRlMjgxMzJiMjc0NWYwM2MKOWY4NTJiZmZiODc0
                            |NjM0OWYxYmY5ODg0MWUzMjY5NjFjMWFiMGI1YTVkMWNjZDAyN2Q5ODJlMzg4OWU3CmRiZmVjOGEz
                            |YjEyOWY4MmViMjY2ZjFiMGUxOTYyMzZlYWI3Zjk5NDJiNzg2MjU0MTk4ZGNkMDQ3ZjEyMQpmZTBh
                            |ZDJlZDdmM2ViYjg0ZTNlYmFkNzczMDBhMGI2ZGJjYThjYTkzZTNmMDEzMzhhYzIyMDJkMTMxMjcK
                            |ODFlMzFmN2VkYjg5OTc1NzE4MGEzYzk1YjBlYmQ3MzJmZjY4YjYyNDA5MmU4ZjNkNmE3MDJmZDU1
                            |MzlhCjNlMGQxYzY1NjM0OGJmYmE4YzRkMjFkYmRjYzVlZjExMDdjZDU1MDZiMTA2ZmIyNzk0NzAy
                            |ZTllNGJjNAo3ZGFkZDk5MWM5NjI4NWI0ODI0NjUyMGNiZTQyZTUxM2ZjMTRjM2NiMGFhYjZlZTBi
                            |MzVmOGExZjM0MWUKMmYzNWNjMGE0YjYzMGE4ODQyNGE3MDE4MzU1Y2NkY2Y0Nzc4MWNjYjYxMzMy
                            |ZWViMWQ0MTMwODU1MjgwCmI5YzQ3OTY2NjU4MTliYjcyOWYxYWQ2MzMwN2ExODFkMzdiOThmODY4
                            |NDJjYTI1ZGUxNjQ5ODk5Y2Q5NApiNmU0ZDE3ZmRmYzg3NmI1ZTA0Y2Y2YjhhNjIzY2RhNDk0OTUz
                            |ZmJiNzMxNDQ5MmI4MjUxZDE5NDlkNDYKYmYyNzEyNmUzMjQ1MTU5Zjk5OTg5MGY4NDA4YmRmY2Y2
                            |MWQyNTVkN2MyNjQ0ZTEzMzNhYzRhYzk4NmExCjA0YmI1ZWMxNmIxZTI5NjViY2U2OTQ3OGU3YTdj
                            |ZTM3NGE0MTVjZjc2Mzk5NzQ3MGE0NjVjNTcyMjJhMAo3M2U5NjNhODU2M2ViNmVhMWI3NTVhOTgx
                            |MTU0MDk0Y2E4MDhkODU1YzZjZWY5YmQzMWZlMzFhYzY2MTQKZjVjZTczNTRkNzViYzliYzk1NjJh
                            |MmZjNTkwNDI4NGMyMmVkMWRlMjliZTY3ZjM5YjYzZTZkOGQwMTlkCmRmNmQyODQxZjA1NGVmMzdh
                            |OWQ1YzVlNDJkMTMyZGE5NDRjMjdhYjgyMzdkMWY5YTM5OTA0YzY0Mzg1MgphYzdhMDA5OGRlNzhi
                            |NDFmMTQwNmQ5MzlkYWQ4ZjFjMGQxNjQ0YzhjODdhMGVkZWNiZDhiMDAzOWY1M2EKMTE1YzhjNTYx
                            |MTllNzMwZDE0NTdmNmU5OWM2NWM2ODEyMTgyNWZhMzNlOGY4NTUzOWE4MWM2M2Q1ZTg0CmI1NWYx
                            |YzMwOWY4MDQ4YTY3NDY1MTkyOTE3ODFlNjU0YmZmNGZlOGNmMDA5M2M3YjNlN2UyNmNhZWE0Mwox
                            |NGE1YTcyOGQ3ZTdmYzBlNjlkNTc3N2NjYjk3ODc2ZmE3NzAxY2IyNjE4ZmNmZWI3Y2Y1MDRmYmYz
                            |YTAKMWFjMmFmZmY1ZDQ5OWZlODFhMmZjYjJjMzBhNDIwODAzOTY2YmNjNGIwYjdlNzIwM2M4NWZl
                            |NjM5NjE0CjgyNmZhOGYyNDZhZTNkNTIwN2QwOTVhM2ZiMTRlNzA5MjBhYjA0ODkzZjFkMmI2YzJk
                            |ZDFiYWRjMTE4Mwo1NDZlMTI3ZWE2ZTkwMmRmMzljZGJmZWY1MDBmODI3ZjhmNDg5Nzk3NDc1MTEw
                            |YmEwNTg4MWViYmU3ZGMKMjliNzYxNjc5OTg3ZDdjNGU0OTM3NGNjZTMyNmM5MTc5NGQ3ZjczOTkz
                            |MzliOTAzZjk4NjBkZWY4OTk3CmVmNDkwOTgyMTY3OTFlMDVkNTIzNDA4Y2E3MTk3YzViOGEyNDIw
                            |YmJlZTcyNGYzM2ZmYjZhZGYwNGNjYgpmM2YzYTFkNGY2NWM0OTUyNjI2OWVkYWNmYjZlMGMyMTIw
                            |N2ZlMzNjMjdkYzU2YjQ4NzgyZWQzMGJkZTQKZWVhOGVlZTkzNDg1Njc0ZWRiMGJiNTM2OTcwN2Mw
                            |MWRhMmY4NWQzNDA3ZDZlY2QyYzdiN2YwMzRhMzA2CmM1ZDAwODZmNzA4ODA0OGZmMjdiODIwOWUz
                            |YTVhZTg2MzIzNzM5Y2VjMDUxNjMwMDMxMmM3OWMzOTFkYwoyZGFmMDcwNmQ0MTkyMzk4M2ViZGJl
                            |NjUzZWVlMzMwMzYwMjE0NjkxYWViZGJiZDBkYWJlMzg5YjhiNDMKZmIwNDFkMjAwZmI2NTYzYTM4
                            |NDc3NDUyZDk5NzcwNjE1M2MxZDNjNjI4YmQ5NTQ3ZTgwZjYwZDAwMDAwCjAwMDAwMDAwMDAwMDAw
                            |MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwZTgwZjYwZDAyOGJkOTU0Nwo1M2MxZDNjNmQ5
                            |OTc3MDYxMzg0Nzc0NTIwZmI2NTYzYWZiMDQxZDIwMzg5YjhiNDNiYmQwZGFiZTQ2OTEKYWViZDMz
                            |MDM2MDIxYmU2NTNlZWUyMzk4M2ViZDA3MDZkNDE5OTFkYzJkYWYzMTJjNzljM2MwNTE2MzAwCjMy
                            |MzczOWNlMDAwMDAwMDEwMDAwMDAwMAo=
                            |]
        in x
        |])
