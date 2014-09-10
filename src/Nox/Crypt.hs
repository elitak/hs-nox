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

data FileType = Player
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

toWords :: (MonadResource m) => Conduit ByteString m Word32
toWords = do
    maybeBytes <- await
    case maybeBytes of
        Just bytes -> do
            let (e1, bytes' ) = runGet getWord32be bytes
            let (e2, bytes'') = runGet getWord32be bytes'
            case (e1, e2) of
                (Right w1, Right w2) -> do
                    yield w1
                    yield w2
                    leftover bytes''
                    toWords
                (Right w1, Left _) ->
                    leftover bytes'
                otherwise ->
                    leftover bytes
        Nothing -> return ()

fromWords :: (MonadResource m) => Conduit Word32 m ByteString
fromWords = do
    word1 <- await
    word2 <- await
    case (word1, word2) of
        (Just w1, Just w2) -> do
            yield $ concat $ map (concat . toChunks . runPut . putWord32be) [w1, w2]
            fromWords
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
            cryptWord mode fileType
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
            Map ->      [str|BUC23eG0RHggwqoD3p0fRAMMx3KKGsY4X3jMTueIPjWPnQPDNqaML86C6z8T30MlY0SKKDsFLL52
                            |oD0LgzmSgrNXfDVw5fsqrvU2BZ/u0Ij8U806ipv55Rvmke6zWBi4UD0Z7nTTdy0+4Ax/eKKB3eRj
                            |w+FkxU/KY7xoQUwpvSpItmMZMjV3KbzxAjuxNxG/qaU42gY+sv4KCiR99cNyi+4bPWbG6NyR341q
                            |1eGzgoxQgym0VlETg+AMHuvEktYz3/5qiXYERhO+F3vlIisSbUOxcrw4yGaYk0aabBWwyE5Rrp+G
                            |DfELvv6O+QCF3FQva0ZvuJiZpxG46deeMGxVuS4m6svJABCJ/99VNamaeOFwne7RjtpkN/137jNx
                            |Ej66x6CztukT9KETascjrJdZ3EAcp7y9Oq6FyTDMcUXnzoyyn7PfP9PoJoLwD9XeOjKUJH/gzXA5
                            |wYYyeP2pWgP5YKbRVhs+viwS7QRcjFLTuOjFRfoWfHLGYszWQVjt4vYK5UHYzG9b1YcdMa4o8s9t
                            |Th103l5vjZvwuUVkIa4VLXuOABXlDBMwV58gRIg6dlHZkKu81CHRN/g626WgbjCGLtYb+zbheRgB
                            |vl6iASYglQte+pjkIQm9RPzkCxhWEI6fWwEtOU2pFIYjAx+hzBnJYCOTkKHPKwYVCyzR3vhW3INE
                            |5Jy3G3FkBFL5Udqa+qBxLd2YDpUSlf3Qzcmd9jtEPEkzoMsWUsvi68nN9VBVXwESartm6UhRXYId
                            |uXEyERwv/Ock3frmfmTCZdhfG1o5yFiHtqqHFY2Ze3iFxylxRpLIvf/feZ3CibS/xfsHITf00sS3
                            |PM+iK6aRY74AQ+Q9cTv78WgMatXEEs0pblFKb1Md+dVpfiC4pjJ95Om/YuV042K2DhkNa/IxYuzz
                            |3UStSzQvmMP3CC5iZ8eyN3XbVyCqVIGur5SIeTQE7zzgs7AbhZtYIwUkaI+hAFfWrH+cXAp+7dDI
                            |XBO0odYHnH5mHvbL6ToWRib5gLvYvRM5qrR7QJBLWccgIqb+g64ZQooGakr0vXie2IURrdM4/S+I
                            |Ya0ocD7ahb+XjTuik/rPph+Jbza6dkiWzytZbLp/MTyS+tkPcBXY3m2ddgcw5SntT/TwJBRfZfkO
                            |5ZtoABqvIC6WRdGCwkBEoHi06Nd+TqnGrXtXPAre3kkNhcC/UrfCvpOPl8a0ysEWPe0hh8v14Dta
                            |ipku3F9t3AefsMqEQ7iq+RlmRaOQkcaXKiULCHQ4cXTSLl6uYavoAI6XMtmn1HxOKM0GHzmV4Tua
                            |UObK6mMDzk4gfJMVxtaZFOrCw0FueFglLpc0eUn1Isf3dr5tNSGt8dKBbp/w4ZpzsDBcFM0S3XDr
                            |DprQiZbGNF/+t07FJ4JxDo0PNqPqN9z1fBXZZWCZCLdGUVzvsZIAR70bpyIi2L2dabf/MJxQautZ
                            |h03yPrcxvfv4lr3/VbNPlfY0QLtzZR9bsGKOD3+f58pWdxBi7Mkvkqu3c7FCJ1jZGYIWL2pegUBE
                            |sjHbRVKYEWpR2YUXsQv3SW9DzKY9VDLi150xqbrP53MlRoWPWAnBLNf4NNi1no0EadILfpu+FXL6
                            |/KmlYhEAyhQ0UttG9u7C7FeYXZE3Tb+1b6aUwD6V1JU/JMWmAk4emCRiQk7G/DjkIqUEU8i0Im9y
                            |7IK4ire2d9MbDBPIIAbg8Yt1U/iN1JGN/hQc+YvR4OukM2+nDShU8TRqWKaky1HfxhLgGUvodX81
                            |VXLEGm2VbtY5f8Z4dae1sYzHI2LFnrnmBFWTMHIVrhzgI02RLVyRhNpTsqTp3/OCmVMnI1oadR2k
                            |lFkzhusg8e0eVs4+arbB6VO6UDXix4EOw5cv/4CQebIFDzCf3FO8pnDlVaCUMAzNKq8txBW0rnNx
                            |6nXPiK8hDysDp4hwN4tmzFJLCIiGi+x+n951w21ikFlsWbfbHggz8/9bm46O4wZytjAQ6X32T8pz
                            |o2Q6tvfRVFUSlSs3Pz/SsE0+NtbrYW3q3qpXA3msWlyN3AuWF26sFInEcJrZniCev/B5DV2UYQ73
                            |5oFQ/ELRHE6YmHiXUJ1gzkqEXSnuyHdhFWHWfgoKsaKC8jyq9Xbu4+cL/ZP98W2/nvfVfNB+xLBI
                            |iZgvU9Jfl7sky/KN41FAkCBlaEZ2/9js5ZMSMmnF0wptkrIckUR4p4D5Jyd909QqHfEIqb5ePvJ9
                            |igTTPlbmQE7h6sJdsPDcZ7QeDgBm+3Ej4DlJeU1WtoIRbKX9Sg5ZipHeqOmpKC4RTsfm/7xQZlsq
                            |/vsfG4Eeytgneg+vog9Ve0RmPcUoH9/sV7Mqx8UdfgS+BsowIYtUs368Ri7NRpC8XNn5KikizfuO
                            |AOUsS7y1UfMW8YdwhiMC2PytlUvyF9Dj9uEbyOvHrroetPEp6IKAqAoeJ6t++bxllzcfWbUPGFGa
                            |6kybyV2IkUFKkYGuvhlSisLRSeUC2f6xmwP9qejlMY5CVMMXy7OiGDy+681JKty5aM2EsF3AK4ht
                            |16m7R/zqdUJF0gu6csErcsUVfMJBl287fShHqTFyLJbCic/UsIf8snhtCZ0RVf2G2pqTHDCL9f7s
                            |Jt2g0np102V12vILCZ1xIL0uvqDTfilZV6CYVk2igXBDV8xJ8jbfDlKtTjrdhV+rcaclR6Pr7Csc
                            |gzhcGc90WeJpSFTACQXeyoEs2YqvFMKs0+7cP9WsFiQoc8TwGtDrN2g7IX6Y1vf8p8iwD0olb5+c
                            |7F06mDjBec6RWp0QrUSFWjpms4QsVREfRN5bohBaGagoT1ZdRQdA2IlQyYvFG1JfTR+7ZMnmuyuB
                            |cc5QspDCy4niiTOtSu1mXPcH26o1DBp0IEn9KhDLXgrIWCEmM63oVpATCH1smvoWarp/B55N+k9p
                            |kFlD9i9UyPf/1r2e3PqAV5pl8ADNd92udmrGrJkrbz3S7hfxg77Ev7A1qdhVZGDE3/q4T3BqXrNH
                            |95RFkmJhKMUSIYmkZQAgQSgV6hEjKvaysS4ZVkgPXSXN8GxnkpRg/r78WNum90en8ZVnNsYKb8g7
                            |n3dyi/Gf9UYT2vGkdP7qa+U288np0udgbYwlQ/tUlzc4ZlDl32i29CY25YoNxrSztYp7YWzlsjDn
                            |A6vTTmzo0OUbY4gc1NP37sUN5jZ1Dvv6m8nIeJDCG1Oi/3i0wI0yvGksJEdxGnrAFgFI1lrtlUYL
                            |kCyyiiynkZGvEP++MyimQaCjBER1BqSlAHL4FQz5PppJ5mCsprrmkTwzrTcQzHq+1E9+E6jiDzi9
                            |LLZkzkfzuEEhAo0qapYvIhxKICWAJVFqwY9EQ3MQay++hrD7+K4EDCo8S1Q0WM2S776XBwbHK3HN
                            |TXdqr1eLUcu/KlJxxJeo3aMa7Wp5hhuNthbXHGKchE+6kWcPO8iESxtvjDXa+dvxyrcR74Qu3ZUm
                            |Xcze0g+FwAfcZwjJxOQJqriu8PNqakRAnNtiyXzcRGpZy/cKULEmLHqCWL7dPjMTdm6u6ytVY0wn
                            |mLZlKZIyqviRUF0Kyw4kxoazvbF/BKY8A/+OjIZjOsEubh+NuzNHLlQrrHqytd0qyI9xcbcIxTEo
                            |alJzHy4QK9XoM5U0fxtNelIPqGv7c3qdgyRJLpdEMReWUUD6wzvHzmu1BvjIXHdpuIZ0ypvUhxb7
                            |dV7ECPN/Tj47iHEdF6BCPc17h/XSg3yLkF04W0vhx0utc4dPdaW6go9sfIc8YfmEOzagSrWEdWiA
                            |od9Q2Sxu91NhoFt1bOrCkpZCZW8SLJaaHLYm6//O8g1Ov6Qd8EA1/KKzyGNBaekwoi+fg2zRbQW3
                            |PJ21nSZHdQVSfnmqVCTjrige1oVrWp3KCjaL5uecpUX3efpTcP+cNvFZa+ng2oV6XUzjt5tJkp+R
                            |rqtwlmmhR7W9adJlY6lab1JM7TSeCTCEfkfYEld6dcPqyKura1dI1e+5lZFo4qoJYHY68DLhT/FV
                            |iocAVsgt5MHUaWlSLcm2Nk1M1V3CggEuYU2qrVqFGZXP6/nvYz+itd5444btsFVNC1nVngGVc1wo
                            |VpxaeDtYh+8LKHKDik0K1Al4MNRG8d3BGtRspySNHO9xfx/I8kR2OqK1uczgddEhqjWUCKQIeIbw
                            |EEs+KKgVExWft6BdX7IKLHLiqngpuMuvpZb+k8UHPRn2Vd6SiXCbojIRwMhkPc0VIPGvP1NwH2hl
                            |5894bh0DNtcMOD2BjCgKcxYE4TIhcBpC5E8TQbz35e8nnpAl6d/c+9YmXu22ETKp1eIt6EDI7jon
                            |MgPlGe6sKybvnAtN/wbUHWNJjXrAQOznhwuZ7DOXwdoLQAf0O1XxCxn7/VMS4zWgfFkjvUI4pVp6
                            |TdNyHKZChGn5hLM5JDopJlF4PoJrc8MYFVRheS0m+VduWvC+PCinQkJzCzfSYhCj+2ByIjMJQuIL
                            |WiIuT77vkqYUca+4zAZ/aMEp/MR4Rpgb7F3KdjvhYLnjLTZrdxCD2OPUR4fAfPT3gQUcp/LjBgrx
                            |U79tuVgDMbEQzsINYtKNuPi+MIWRh/JK6dVZ9s2hVeLZsPN3l/maHBgUR84aHNH92BkEiFbI1LNG
                            |3G4JicyaEYn0JyIV4TQNuThH8ElR7rHMDVONLZCn4kp8EEZ8bTEI05BFVDT5n0RMRDU0g9urYtEB
                            |KytahNwyJAGDV2bFqGzv/+T2UISmzrmLWz4LTruniPvFouHgmesYa2AQ8bsj9oni40pyrKvLt4Z/
                            |gvaSmImEUx+5SFlGzKZbaANbUrNwDeLJkQXzpfNLPkKQa2DIcLaAH4bwwmMPm1tMPzca300mVM2Y
                            |bdxHRUDoAIxM2HwMxl9JsmGbAQgIq3SWvWG08Pc31toEUHJyrpRl+xPdbbhBb5G6GAsvZuvhOSEe
                            |BHFz0IYphpnUp5lbTwBWaJoYKB3zGeDBMXuqNV7IxeeeVjFuL0Nv6ZprC6D6stRqatr9JVNYZWYh
                            |Js4Y9ozwKZ3+yHHSWDihyy0ywmQFz8eZmkxHUTVwWZ0Nr3l5G8jAd/nQVz7mjIT+avC6M5ao4nnU
                            |oB4c8hLkDAdb6vNZKzJcGev2u9Iy6P/3TiA9JvqAMelV2iqzHz5BynbMLz2e2UzqkM894OCnI//U
                            |a9Q4oDMGHCZeYYABiUK/Sq7ffgvF+xD8VvQ7TnV54zbmWioSlEEPiAkzQ0VxI2WbX2Lo1W99o/Ma
                            |22fbmFQdhBPT838wyaU5DHsPEJRkdovL3djVKQ3IYDOsZ3X2oWEjPFW+yyT/fzMdhJz22rfw2Fud
                            |V5ilmFDQwokX19xifD7DAir2WGsEsr3WhjnpQhPHw8DcDW51ouHIWYXVszs1OierG6jOyUXNebPw
                            |FlhpGi7j9TojLp+O5BQ2Z3sQqd0/4CggoGti8U7YbTxW6dU8Zzz8n3g5O495Gg3wHKYvPXHs3phJ
                            |a0PE5W8e1sWoQPyR3sZWlToTFDTt1XpY8sYkRLF8L+hmGB1rm4+pSj7DiAP+9DHkCZKRZv8es2n9
                            |FgMk4H/KrqbT5IcyocLt5+J8ox3buTXbpPaORG6M+UTbA41LR5y6E5k6fdodtxa71hlSdftEsgD8
                            |hwjTiSc1rgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIknNa78hwjT+0SyANYZUnUdtxa7mTp92kec
                            |uhPbA41Lboz5RKT2jkTbuTXb4nyjHaHC7efT5Icyf8quphYDJOAes2n9kpFm/wAAAAEAAAAA
                            |]
            Thing ->    [str|p0ozqy7zkTnfP0uku6bFYTdM061VgpIJoiqHd1DpdjyJT3nYJVgNy4YekJxVnFSjGtaIRW4hOC9p
                            |Ks7eunSNCWSshNbZ6C+aHAH72qcz/3mmjQbXdjyX5RO1frQ0bvBR1o/BZcvNtio4S8+WQZXkmPtF
                            |+Mx0icx7YWTh55OJMuUpcMQ22Ur39k4W0gLNehzpk6TukOpBmp6k5F9/Oe9i4FKBoUqmyenZ+yc2
                            |ifoEqxcAWR4Crm374MrAB05DkkOj5gBY+0ooerwi7N3ALVOL0BtG2BhDxJUK9u3ymOLv6bY39tdb
                            |bofnZgvyq6IaDBLINtglup7lUEho3ZEEQcDALJsf+YWwLfNE5yXP5uYnLRBvMAgtS9eSgW2TNDJy
                            |w63Zi4OPMhv2Oy0wR5IaE+eB3IgVYMmhAmwVlpPs0idRIq8WVWEaJy/REqn4OgYfc5Bx/JEwh64O
                            |Ye6p8yiG2/G/kbqLDqNFr4ikxw8loixAxrGJ0LkSRVNOeZupGbi9T5H5AB2EYNAlxzJzQMY4inPH
                            |B2/zh59OpUXD9KRdwXf3R/ItdfWxVpXxEuLivf8EctBD/KB/d63q8jgukClM+KiHPMlQu8R5+Sdd
                            |yBZRrok6pOHNPjCpxzVfchavRvv/TNIoB00d+RiQELZ293aghxm44fGrLlTG9u66pQcgMs9HhQ8A
                            |suTySrJhIbSbXXcIS4qvqwGBPQQNiFZAQAzh7ZA43OaDiOwxYRmwaLgb82LP+P6RmZixhMtx5FMH
                            |k6rjrHLOcGoZjsUo9YSoLxQ0e9ag9xUzzQDEihmDQ0tR1IChjSesJqOfN2Y32R0nWvomCkF04+sd
                            |EdRnfcyXAZSnDLdeFGqkVkrraiW7c9pZe8IjcMiBNQsff0MRgO+QEjLJGzUDt3EycKVd9ogEg8fT
                            |QPNv/TYfVFYqulJQtDldBlbPWSV0qikJ1XBiRTk/u4T93rpr6BXQn3L2k8vNmHKuHHxtj2880pGd
                            |FL4nIxU2IzQ78x2xoYP1rZCcPdoh8I5o6xsMgrpfBX6p+2B/kLKdbfNQJcHH/J/UHJ7Pw5bSuJ7I
                            |I2KR0egztri6h9SemvU5bGgpr8z60CuqTKwm5JAKnByUXBTqBSOVZLHyDcAaGhdmaJrZaXwj3zDw
                            |mMPqjdonoXp3dOuQbMiDw0FM3EnfgpCceHsTcDLnrby6+V1ZccjC+edQDbEem1vBbrPiWnk1o5G2
                            |TBOXzwm6dUXVjYb+6IJvjHeEI73/f5dW2D2ApDK0g53bTYbCIPcMk4AwKEanrSvjAp8kDS1rSlVW
                            |gzBu2KnWEjP7JZATEzCV6f93sgHhTDzjUooldhaZP1P79pCusUNxxLWw1Uap6GImCMnmlwFrfL6Q
                            |o9HHBKX4Yt8ktj6Te9F13XHeqYijaGDdjCIA21rM/FRxCm+B3CGagO3+HFCZf5NI8PKbSlujtGNA
                            |dgwarFNUA/zN294BvzSkFECho84slqY/GniFnSG8JltckGPiOWW0oioqdvqfeU3aEfDIXRj/y6lG
                            |hVTnZQoHdpAEVtV55PJqI/fVDHioJ9bPr4uoi6AI4P2Jy/f6+0l8yg9xt8pDoXKIwt/lms9LA9z5
                            |XhisNdpVtKS51b7HBC344rJecKJqH3c+UsO3knHrygR/ZWWXFc2WzNJVy59+kc82p2wX+qbJhkmw
                            |3SmIwx15r+4l4KidI1jPJKWbkkGHewQ4CgdCWYMucgkfdgGBMNhQFZhjjYNCrZzFKJs4rvXI3CnV
                            |JP5kD6Xa34DnTZeEkzH7dA3UbfD9Vwxe9j30tC8yNvVZQyNqoOJMimNhd4xVDS5hzY+P+IE52maG
                            |1i6C2efKIaq2fAb2REQ1Ki04inEU+mPApU+lsYqcqkBLCkDFb/dMHvFNBficHgs8wseXhuXK3Z6W
                            |R1jIDeeB4fedeB0Bb78rXbtDQxTVnnKl4bIjB2hJhuS7z0JNIqn3vpmVdnSGU5UhteSziDtQZ9dl
                            |nzQr0kSKma0zaGctHn+7IWtm38VSCh/YSAkpv6S7m6haO3j65vbe6GUwIL2jVhx1WxZVFohZeoNv
                            |9XV9qVQ2QoPHgw1NPypFEdOKYSO28l0UOf+3WK93hFKhG4LbNE5XZpWr/HJ1ZiHnD2Pi8mB0J6zU
                            |25sbjHvwHX4CjZ6+67r/GyYMuzvgCNPuTCvle0fnZYboauqXRU2aucMYreBLmW6hFNVx7cfpiAqZ
                            |E8ugyyTb8kcbH/00EXCXlszmHHQZtzV/pLZOxRkYssNt82qx5SpqmmsfAuldKFZrkETKI08isyvU
                            |EdzpOUtVaUumHeAXZAEW9dujnZuuaustzrHthlQS9kvqqxA+SF76vloU1z4kfh8u/YdQJiGWYpqB
                            |hKs5dsDQqDE2BlbWeP8/wMrRMjruLCtVanKRaW7wmEduFUl4TXPbeu+ihLjdMcQOh2A07byvVT8s
                            |n9oKTGedCBrKBS4JohG04huy3L1WYKfsiFwAJRvh85bEzDTJ3QN2WSpEwaPynJS4U8FMpfgn0T4+
                            |me4hiKmOA7JfeqstO6K5RaE3vn5Xdjt6mU0h/suOohou3OhXDMO5VQqAT4YSn2Eqrijq+2Zjtehl
                            |icA7tcxGL7HLu84Vvq/R8zjXTgVAMYIe6ZtrkQc+x7wbMfJ4eA/9Nzkg9Yyo7ETnFn/gYJ1WZ/Fx
                            |guQoUC8unafJJ76zSIdBgHo4n8DjPazuxiBcbc+7DWTeVM6HkJWi8dSZzbfend+yYT0pwwRBhtpj
                            |1DGg62Qv5E9dz3W9RoYtBFvwABAmrftYb9nzZwwL0BT4DsOCpEyP89RzM1ICnovYCKgiIgkeQvGb
                            |UQPmQ0u+2nQxEZuHJmHDbQmH2Vutt9OyNXxG0xYEwENlKMhRFXC7HkCXl+upyY+4ReAw2GvAoIyd
                            |4DraQ+qZlM9Rul+qr/HrH7fg83ecyjbD13tSfok+FjlaVEUVKWZGvtScEZFeHzG0yFS7TVmLJPoR
                            |WJEQHQFl3QBbqAfAps445QyKYDnzV4cO6sYGtLGZU2WxaasDg1gS5n/o8Iu898JSK8hbjVLEjAgW
                            |I+1bVb8MMLp43zZTcQd7kT1glmVVE9lb0VXYr0AmrxsRV7IVULsBbljOo4bO3PwYkeyI8x7tCSno
                            |GudpaLpqmhfB6oWxEa+STrsyTuHX6/zHw9WJk55I86G+BUCwWffFxlZGrboYrkmFTQa34KeQxkuS
                            |8INoH/l/SmNfhwtoM8jhI49IRcyddhe0LzjAZGcGUGSpqlOu+dwB3VxKw5Nc/ROFW+ZcvvLV8qMf
                            |i2HeEo4aFkDLiyZ5arj26/Rl0NGdE4SNo32FKNPW8zjBcObR9ZTfjbynf+KTXyS+4C5QB+mqR3nL
                            |aKA/8bk7N0+GTuFKkRcq/7dsCoQ1hFr1n47kX2Eqz+WoaWhckVCqm9cZhVWQDD59FFxF2/Z7pXvc
                            |uOfFfUMoBfqUSLzVqSU/lBadbNOvhRTfw7yzGi62FcxtHYAJ2OhKnWmXeqNzltip2Lf7OH7L3SME
                            |VhPXTcK7zZ4w4Ly2HbK9VrTtWAJ/PIudphVAXm8S/DDn5CS5edKf3kHFqP7kfxAJAK7r6un+KNY7
                            |fhS4CQBnX1zk2HsNWiJzz1+eHNU0vs6srm5z/8d5vtAUZbwrBt+TY+bdKBk+LX5+PlmMCZYWgcdL
                            |K0WahshJzFf4UkMU31omhV6vfHuaW1cb3yKNrOkwz6CNjsy9BqAyDCCwAShg65HL48GsHAioqEW+
                            |huzWHwXJfvBe16uUQZ+pTEUvDuuptfPhmxvmH5WPAIEKP2wMv85Y4i/vuHmaWxlqRRSr0XXEhYXb
                            |cbTpQ7eAUkFzKYwsheMBQTqM88Tiw5T2SYkaapvlQ9+scXF+nwGsw9tzNg7CTGoJ7O3wf+S82gb8
                            |HZCtchIOZHy2Rv5Io8U3P1vqnwtoZD8p+xgtq+n50n5nrDTfEWPY+cKLiGFXHZj071fN4eVFuU8Z
                            |LoeC8ek+VnOysGkUoL6auIzVNbTqrsXDDhrmw6fAbFAhNZAgZ8ZDMZJ5tiqQKtt5WVHpxwKcqrSy
                            |ku7F99nWOW05vq3oums6UBW3LrVXxVCFNBhf6s1V7VBX3rtrhEPJna52Wvg4ViNtZLCdXYtyhfqR
                            |tI8yYMw1dohoyl/cRoGNyKBkl7FxO5HwB2P5wXMx01RgmZIkuGiZ4kbXE0bU5M9/VklK4OGOeeXG
                            |aF/QNWi6IysbA0hSBTzZMvRb/5dNFMMf539FvQsJ1M02f700l5/Hr8XWpi8Qrlb84xWe4mkKVNwX
                            |DtKdCj7XbDC3nmp/akfYLy/ox2TL6Vq4/+m5LmWD087UkvnGSZWI0aut5NHq1bOd+j/4YOV0wKzu
                            |nSIom6+eVxtwEcrJ2vSuD0h3cXN2YYV0caCsBaGrVpfuSqyONDtToAtLteQDpESaY13x6AbBz1jK
                            |Vj4UqCe8EtHu49108TaKJw6lN8C7CMSxBtkwmdPyjgzSjmh8CWLaRHu9L6UobalhbdHEVE/nPHOg
                            |LA+Ff6NFO5O3eakJajX8Bu2HoUJA+m772wpUkdW/i0BrZduDVORfSmd8DutSzPZhtg9j2NXg+8uN
                            |bBr2Kfsk/ku/52y6YsCQuY+tYtGX/EWUI/cyYIWtfQhjywAdqnv25kFhzAkNLVMr98ZXAs75v+pv
                            |PWBkiqx+tNGNe10IB52O0nk9AiILRbdp7ZruCq1zAU3DrK6nG2PU2odj85o2+8JufRI2Q25Az4tS
                            |bYNZ6gWy8k5U8wC/8iwKMLjOTvDNaNwEhIopkX+6l7Zb3cT3n6/M996UaBRzCS4WVplg+zNoSKIy
                            |dE3RVayQCCVKqyMmf0JxxaWZhrmAx3ArRoptBgCz8tRtmLB7xngLPMRCLBy5Dcvqo5oHxCjAlvBO
                            |tSte+ze8dBAFwOSbieddcJds3rErGNRsPvFijzzT7EZXvnLbZYDnZU+Jw63T+ka2V8kJ8FnQCOpG
                            |TQ1jkJ5Oz/RzwSf/pjcfbRrCzOU8mDOPdKVVOCz8f+z/CNdrYJcd2l/R40OZ+b5jrl+OYVMu0Vs6
                            |QE/FBk6NSfBgluuFwZLppOIQU8zvarDr0Qd4hLWVF4grZr6MG59rJ0qjq55zGDFz6kbV0mrWhO7K
                            |iod6E6YnM3Ukslc284IbPP8BFvtFfZC4A3rmYqmUj/RLq8NFVgLEKlfxwbb2LodND3qjmLFL+pNe
                            |4nTXG65F/t2NPGQVb3rhhi/F/FUWTz0jac3ncHOB0Am+p2XyN9LeSw35GFPh11VOuu3BeupHjs+r
                            |FrO1X8lxnNlZneiFvNM+5Ci9zm+8s8j7l04RcSlYBW2+A6rKNtsbk5kPzVZ38wTSqOEi5xHAy24o
                            |Kiv35cEZNOO+KGH0+eLO+fU1SjgTuk7m8XzympP24wpjwbGb2Ax+AvC/hbliOtWViKTsEdWFqYPo
                            |73M+rnKgcXIQqZtFzSM6jp3r3Yj3AnsXKyk8/hSZUuTcxmekK1/+TrrnwkXMgi+FhKATCvuEkkEQ
                            |45rsLF85TO4SHZFw5Jnf6p7PELSMS0BigRXPV81p1g+URtvBxi+5HThblYoM4jfi7iKGp8f8SDho
                            |rNH2V6gr9QAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFeoK/VorNH2x/xIOO4ihqcM4jfiOFuVisYv
                            |uR2URtvBzWnWD4EVz1eMS0Bins8QtOSZ3+oSHZFwXzlM7uOa7CyEkkEQoBMK+wAAAAEAAAAB
                            |]
            SoundSet -> [str|vbeFwigR4WRH007YTBGJmYyCTyvGSLtGvHvcntHYznHihbMr+9icMPB7NSgxG1nBo6LVQC4MkE93
                            |t+A9nBnKD7++h9hrL7dMaXOWgjQMIiBaIlf+bHB8shlvZbiNZqpGNIlUEtYg7ZGYGd78XSK8FBvq
                            |YH6xdYRwoO19SAeZRRCidBs/pG2qAN10AUcdNqoQ0kUnwJRYlQtt4APBcAnAT85UKt1yRJVbTJK9
                            |chsEMtK2phOvl4iQNO1Dv044pmYwO36InpC2Igk0ufjwSPQjVdewD6OO2MC4C15arma/w0GVzj7p
                            |haZolUsca++coJS9ArN1IEbhJMIKvfouVvwz50CnT86bZBsGsfUUSUOTpGjNPO4fV7plh3MGShqx
                            |fx+Wl3QpF/SAyB73RC4aCayGn8e1CEkltfzjcuEI1hkt/7kuKMjZWPSUQNVDiUekzYJboL20oZ9V
                            |rdt1/qRqV+nvqUw36XOhKbKlIB9pr52JU4txZqTbqbGyTYYeuot7VrBOF1oDdPbUahynXWWqQNkI
                            |PKHlNpV1E2eNAp8w+ht2W6dyZ2vP5WLr+oYjJls8YuyEExJsfTtqaednQKjqoNgsgL7Ke6De7A4e
                            |fDCU2eWT9XpFzQWhgd9mqOL8qujxmy/+xnC7lERvDdHcUZTJJoA5OHapo+E3HkTjHCIceTVGS5+y
                            |UlZDC4o1FWrJWIHPwY70Pdl+b3yoUlY0ea6rpmdy1cLdI1R8F5THR9xaFp0B5SLR2hdcP5+LbJaF
                            |3dBen7+tIh44CCNKK5H5IGuHoHb5/daGCwEprotJas9rNHqRRqFKDHS6CDw7/46p7kYS3KeCWw75
                            |xlcjcVgyBpIM9PKe4gXJivqjkupYk+X7QP0O+UQqGsjDQf4tkkTYd7x73oefrHNrvCEGw49OaWUv
                            |oHYcMcc1m6CpwWzB3gp9GfszcIirJHl8oTTl+lsxcCPGsXCJX5UEG6vAUWdh2/HPfqAMzbYy4Xgw
                            |DR0CqgSo18bqfZIEBzIA0q5y+IxiUJOAGBWbekz2WVPLxICZefe2O7PWTy0/FIcYIGysVxBEifhT
                            |G9tKJXhrsD9AkEW2l+45cwyulxBv6np2uvdrRwQLSfy2TPa0t2CdiBcYL4yoqbTD63kCpQa2m7l+
                            |oEhUesIAexPhJUjLImTGpanMKwxtQwLvJgFt0wg6IcDw1sW/XvHO2ELpiqF76Yi+DVqEx6xwyNzs
                            |IHUiQYvIcmfr8ws/5LYwR8qN0EiQgjBWcoml2OaVsGYtdcnqVPR8LpKLzE4WGZuv1KTarTGWU8Jl
                            |YB62jaoU5x/bxvc5HvJCYIV1KW8/tS30ZOna+JypjBLQQjPB5kp92TQ4gxXog7iMaNzWSdRWdSiq
                            |6CWdUw78gY9GvFF97wuSCN9QbRs8ENNBn5/pYOlPS+j7PJIX34Yu5HrIpzSItjCK6AQ1Khwq4cIf
                            |7sbNqRsZ8dfXkBBEWvERruGmdAYfEf3gFt41TSw/x0DDuLvq6ipKj1W7jMKw7IUAy+fvEsM+qCFj
                            |Xec6fTosdGIGqIwDj3LXqFXPSQDjFh1xcVh1J4/ieNuUsLGmIWwldHuqp1VpsLncS54RSkySOY9Z
                            |8ob5ecJ1en+KmLDfNlpOf7rI2Lad0IuRV4kthM1qGFWflMzwxsP9+nUusStUF7ipSenDzmn9vsdi
                            |zXgyHCVkwufDgPoFlYEb5/xRE85HfnQ+sjMxRR/iaCb7rxVBwffqoMnKDYFfdPaUOXSsLfbTvPsz
                            |5fPqZn1A4vuOevQrlFg35HRYZYRgUrgPavaNmsZJFebHl4dJfzRe9SPbozlsIRPx6JSGFtu5xBFA
                            |HPGbNtiOMs9aZoPkaTxWV9SMjE6O4AJ5Oq9SZ1l41MvIewWrZyOHs6fXRoRKzCNFN9zJVlIciQ8y
                            |P0/E4+z/HN+tMOw2wfSn49EpTlYb+91dr+YF8UPN5NTThXTeFLwavvE1hnxUixqhrfv+fIC7wloV
                            |458w1YEGj4BNUioVg2V1PJl+FTf7cIyIRtpZOahFMGdQAuQ97oK6mdVvEYsk10pkYHhXf42enhgD
                            |5ySGQ0p6pVb7D9OnIdIrolrCgeOsldh9bMmy6anjWNAnfCXh8kWOXjbJI7NJ0s1fx8biWKzOlT24
                            |Sf3ZkYWY4NpL6CxsXxcRnlqPZ8ILCnxb3uX3yWAi2eOeyAK2GhmvZjVgk2k9tFOLFD6znq3fT2Zc
                            |CgxZJo5755Tdmd3VIsgIpNfHfx9l7VhfmS9WtyhR1kavRjgTJzwNEmLf86R/wgDXT/JdHoPCuQXY
                            |aH/lNuyUnuQpsmmdKXyG4wXrUME6tlwoXu4G5/vFEecDbvsU+T6T93ZjlnmzcYTKbnrzftWeLTc2
                            |FYLsxMUPZOK04fpfDJJhT7/OuJ9vDkEX2X5tdkdxfc2iGT6tK8VtgkZ/Yh/oGU0HSo3skJJxWd2m
                            |umwv04PRMp1kQCZqzzloQKXzxquVzMGf80RtYjgcJaqSIAlnWHw9K2zOOjG4Zb2zWyoW00mt5q2i
                            |+fJhSK1YfHWPmkBIj7XmZThIj2yadZ0ua73Roc3JeRU0OnnZ9Ezdry/U5J+puqLUKABA6E1mhHlf
                            |d9TqhxF5yQDGT6W4q2vtIMjjK42VqrQ5p5qhjbrKrW573IeB4s34WTNU/1/hU8htMRC9aN6ZHVsh
                            |hI0kJoyf94SG8+nsubkt2QrTilW0PHW/dOO4GNn1LpnQZYxmZdUBS1FG/TvyQILG0z+YsbYL14LK
                            |25oTtR4dnfvg96bMYAbLrJWrgcEsxKCGmmn8LpQk/4Zt4kTGqoTZmUIMnouyvKC4Z1zfY3nvQOCo
                            |XnS+h6MlcqrqOo4NHFIhWpwQR6XcGI1mbXyNYE26fLngFOkCm41njojUmCNI8qHgmG5mfIV/XcZ5
                            |SartOJF0Pz+Z0lRo4Lb4AyGDe+fBokQdiGjl5Pbf0yv6n/BfQvmv24mPrZ81VQkNqu2eEi60djLq
                            |EcbY+XLJ8k6E0WMKO4ZA0Zh1xmx+jvfKHOpqpZVuax4lP/YYqECwRuizVPKKr9diyYsH9qNDZBjG
                            |Dg5EqYWdabIn86cdQ2zpLSZvDXHyoRhfBrKjD+JGaX/5oxLrRpkF/TZ7MSxDRYGqC0HsL7BUY4U0
                            |MqEONsxwkpt3yHJkYoMJnMYyjgfP76fa0Baw4pYyLyYa8nBYkKiyk79q2nf+lTeCrlT7DijyZIkY
                            |fLmLKO4RUyHHejqSwYUlzh805QPK/YySymuro0ogHzF81CiHueAjtmBiNOUt8gLluFhvK0hRGGb/
                            |LcUMhhEmutp/XGqXM4cGfgE0D3tuF5DUYHnyEr8uSQwgNhpHC/UEXV+CjGkDu4Fy3uLqphHzaayp
                            |BdgfblxKeyFz5pRCfI8IYgunx610e3v2jzTbY6vzFyN79ExlILekZ0KLh45gq3KQ+U5xom2FcC/I
                            |NYNezHBhiGdYxuSaETt6QeFqT/ue+01kGUaVKeRRrhk/y3USRSBz2dcomLpZkj8dGt5+dvb+olpo
                            |BFTXEAT5s8J30QH4ARHulHFD6Pzy7fjmCTBuKzMXg+/tZe4ugaIYGjEz4wjU6kFC37a8HeqzfSiP
                            |eoak7jTHzk+Y46jzUZNrtdA6U2eaM8OBvIdUOgN+ITZptD7bORdeF/mgviNY1Sp4spyWkJKzzQ9p
                            |aeeSv2DJ6nokxvmXw6peg0aa7VpuMlp8NNA5GPEQbHcyfKXDxBtzds28v2/nBo8g5EebXqfBfE2J
                            |RINAiDKu2x1m5kqLpKMxpVTNTsXFz/0/uAfcF4SeMpEaGhNcya/Tldj3WOzqpXkuXSg766O66Hu+
                            |z3NcXmSx23YUB26Viee+4vYuoF8F/6ohAEQwzIVdWnoqpXXvAgqt9TZEdOeOPXNIATPIgZdp5J8U
                            |lcE3XvtHeciNMWqyJBatyRA99xwqZ1NAGCTaJbxDW5CEKSkAQLNszeT4Eid04pctO8H+5Ok4oGVI
                            |2itPrCUcgWx3XDRkpoQ1nqJO+8qVwMq7HYcX/SlS3U1lWD1K6NNQeyzHYhu2DExmZWKqCNlO7b8s
                            |7phUE0pMaFCqUsX2+1kGJOmFs96ZQFHVvlE9iFGUPv/J7w0WjYP+tIO+jm/F8Z8cALsA12k++FWc
                            |LNzv/jgRS5+68+2dXsAm4galKGfucYkVMBSdyE5dgstUgqlfW/0zB4jxRal4PmFuqnJzKGNswzto
                            |yXOB8D9ca861EyJQWcf6thw4XVsYZgRexAlU2Lm2Nb5KuUM021zz+GtziH0apc5AbZPznbiIEGOh
                            |9Pmk7deiXMGou84VbTHor3ShezdRaGckoS7W2rSlMzraTc1Flm5QWCXareT+6FKxIMlUuHOlL6qH
                            |5qnXzrAzcElagAGbVpAIqeqyWexqDkh6OnHUnzMlXFTFmQ9WcyAPkwCpFUo60E0Izz2ENnroUbTC
                            |iax5uWjXvbMVmOQU0krCQSG2k9aM49YHuaORdV3WDW66tgGAMLuOVKe7ABJl/IN0qP/9Y9WM+5ZV
                            |R/wJLkDvWtZ8lew5UBFupjIaZ0E8+7sISviH5rZ1BnGWVl3fp5+xZslcqCt+cXgVXwgpPpqcRdWT
                            |lNcFt30FWkuRBfStCEzK3FkEUPcvYmKPW9d7XbkzpvJzNJQc6uuB2Wl8yNLh2aNn+IKt1bLXAUkA
                            |IrjqaJvZvl98n+T/tljrBD/7EmqES9OJAxfwT+htoE3ktnmUB3X/GX3klZiL3tm0gqsXKG7kmOuU
                            |7JAJY8qp0CJynNFTf4+ZW5DkiMqSVixgn3avoJtQuQkw2s6Q1Vno87W4t6KMrnJYP7EQ5DTzRkyR
                            |Neei//zV4P2uH/eukYZn6cZEjZbRZ0KywFnbG1iJEt6wwULhXrfBsVYzfM0YNg+JLJ9gTjDV7Xcg
                            |emimV0skgam7ukthgSakKxobbDj94RT9EA1bOYPOEsMKsTeo59B3uEhmNZ19O1+sVxpcQg/Brdrs
                            |6yBoGG8iIZoqjXYEga874KsOwV0MVCDFqmSlHIOnzXMOm+kw6SkUkK7UD4sSQV3MG965R3YwxVFd
                            |VH/Bsu+6Gb+V9zcPYWF16njrgbFsvAC3qMci/M/8T9drDHkDVaAf9S0Uw3ILGVUG01H+3Fe3GzIq
                            |PgU/hGoVo9+M3hc5gcpM5pIosYS1nmV/wfbrpIc6oxAXgkdkUjsdb1lHUBfVrrY/aI/hydmOnGUi
                            |mnGijl63LGOxL9biVe8Z2DHN/THBl2NGoXSGrp5Mj5+mHASUCLo5ti2lTUiKmYbhw7fjpbB92LJV
                            |5oZgw38dsXOodASu3xSBVNPd1StW9jFKFZie1A8pKEk6NPmt+q3uR3XXv3jwpSb+scMfxib1IjyB
                            |F0BgPZWNwWWprEdrxaTmOlbMyTFU8hs1ThlU6b3CelbzahxZPhCbxhOyWzMNcKpWU8q0wQ+DyXEC
                            |+9gI6rwW91Lt3xymOg+pArzmBH7TQQJssgRQnGn6SDOAdUpwj1bC23EsyUCTqxHOl9XIfr14FitW
                            |W3AmozxG/zq76P+6VNyPi6Rcw9PRdoNVcqgXtFg2UtocD62Qiiabbm/5KPIGpMYZfsClmrNnefrs
                            |c6kcJ30FYwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACd9BWPsc6kcs2d5+n7ApZoGpMYZb/ko8oom
                            |m24cD62QWDZS2nKoF7TRdoNVpFzD01Tcj4u76P+6PEb/OltwJqN4FitW1ch+vQAAAAEAAAAA
                            |]
            Player ->   [str|Rr9ylU7hAHFRFdOpojlAHYModkxgjjwBErm+fcDpTGBZIE3NJnyJwEqeM0PqmcLhvk4BgJbHTmwO
                            |gp7IVupsnzv+IToUoXE9xEYLyYVPODtFjEixPgD8GeEK9tCLGi7DrkxtpkzLD7AFK/oyDSHf8Oxo
                            |2MqQVWS49JUYWhJoXNO8/mkoQr8mIJHvgmlfpBJXRvjWJNU2v/eSj7l7GrAXRWGgOUR+LRv8tlEs
                            |YIgtN795HXeAPE34lGf9waHRXQuu1FJz9OvCHYoZ+aqhIJCvh4bHGCRbwmOyNILu8wklauk1Cmzw
                            |tQ8n3hAdec5aNvZglnczGHmbxI6JkP5LTvGLpw6nhcqnlxf2LhSD0J1dEZAvEf890LjDiOH2OeH5
                            |Vx6z1Sqco33/EeIaUJ1PZdSZPRJdNl8PXBd5BGgUNumeiaN7fJXtJJFOLgTs1pGwZLQq4KDmRY54
                            |rvkYmTj5piPSnUaEaiS1+Hbmf9pJlfan1DtcAzPNTK9WpfBD0GrbYMuOWrChEoD0njtCVwIRrR/f
                            |n/Xe0gzWSdNKH10ZV4XTHSRaD4t0Rxm6vhvrQ8Gxuv2jv1W5boOmTGoekQO61Zx8zcJp1i7i54Q8
                            |N5tvui1QeLb5aTPQIb5zVzi52wI6rgHkz7l7rVdFEbphNHYgm9M4ShcSOEEBC3TMQ7Ek4w2J0enc
                            |iXF9RhzRGnH/EQmoKw3MLFjUGnCRYE5sR99DLdKRs1YMrvja+BkyWXFjCSOIOjtXUCIZ2v0w4ng8
                            |wJHpNM5wGoPCcewgMHnKKvA+CavhhPyokHe8OXRSaifwFdG80serDElXmcpMUgZeN6H34dbWDGDn
                            |FbDH0xq6Zc9Vcen4w5rK/aJrBCEkFDdHCnHkJJMxlpRMt6o659b09wEIsiJNBZO2wDZeZbsAc+GW
                            |6GiYWuGQUkgxDizUj+z25jmC9ulKDVQxBjh5fvFOkEEgeJOVRhA+/uUbbQrlcb1dGUwe7H4+/jJR
                            |q7BJZiQ2CS3YwVBj/7HGiJ/25kN+NP+a9d18O6q+D5dIDZrAAOhnn/USSdZCIFs/nxTsebRSmDgY
                            |knVfr91JiENlWz8x13iAC5LYBb/sHrsIJZFX13ZZGvbXIvEUeFHB+6e0ZlfJ2akQzbRxmIbnOvNz
                            |IwwEuzggWk5s5M7ZIewet9+i2WhXwHy8wCTuGpalLmZgxVIrVtgpzb4BkZBYNfzCQuAc8P9Q1Zr0
                            |ZGrgoD08xFljibNuUnFc/MTmnHDWYffMa3PsEHoDDDz2gkOwZZ0rMD95Eme6toSAp79tQR/ZkYFw
                            |ispRtkCTCs09ehPFmxL6t/yRBioqGh4ycUhXV4h+Qhp/33KRLxAaZgQuDTQDenvs22qkx/3v/rMM
                            |ftae9xlgxpx4T7TFy11ucKfiLH2Bsqfmz2YFHJFErCmQC+9WjiYlgYRvwhYcPhUL2qH3OBg1r/vY
                            |iR2xhLEZEQIjwjcQef2bgyUILtadSGiigQpvc7H/rfQ4hEzWezISQE+tVf/LSrZc8zAkvd2U2p2d
                            |7Ddzc8/6QMvkQ2GCLJlE8I8FkkLkJ10yJWVDazp+ziA/QvJK6unjkf6ErQP3Mx90CBobrtktPqT7
                            |qwlOKDAE5Lr3hwgCUNtQS6LwZsEWgMvBcN5HT16WatUaQLbT+V2NswO3/+n9B18LIeIqlGKQ6Ik2
                            |2cQIB092nbtqW5wB/QVpCDc2e0pJVJ7CNDdGkXufl/OA1pw+2+7DJuVLklV0hg9KygYcVcUlxW5T
                            |HoFIidYFuAWw1s3/wpAsr0XqWPME4tGxdTg/bh2jKCpXNWBB65Cs0HC4PLsSv8mt4odXZvP9grxu
                            |zCwOmaw64RUHJV2N7bBFE2sII9hP2LSZkDNMGUzPTRJdkQo4qrNDVPRt9XWP39PgQoGDK7emfbZj
                            |1P0/y5QcpoOOi9bFuO8Yrdn9+N/IERfCXh8NV3jKIVq4BTEeWJxl4jM19M++2ztAhtYC1k6pu9r1
                            |CCQ2gnahzcbuSfY8BoVzrFTBtqdACnq1XC0tSG0SCI5d2kuDgpYF1TTv/E2gBFBT1eR0t6AVUnz0
                            |fVJhS0QU1wCNWGctSPu6kXyedouDClG2D6wMcnhDTtos/KCMJJ+R479TieNFcoXffAPGOYlYMJWB
                            |Pk+6gmH54qkfUU/c6/nEnRgwLH7LWQOBQAOW45QypUJbg3GXXXuCslPnn88fmrz92n23LFrz87Gx
                            |EwS/JRbfj+6qaTdh4Suq5XeaAWsyKn1Crl3+7WaEG8ZJtw1F5FrzbH5mGpcCQO+DwhdfdME7br3d
                            |uRMcykY6GLqmCFsD+63NdPZYO9UvQyiv5UgjYidZHPeIPjYbiSgO2/TPnAqiwpXP+deuBPcHcidF
                            |GSfRslRaSyr4Rxf0fXqQ8J0R0ClJ13/FLCp0+ACfgnbrtiwGYLXv4/131iPUr2t9WwkeqtFJ/eC1
                            |5jcPLcSaB/KYzQ+wFfZosQ9cpk/QOBGOhN6zKjPjvJQ0+HJ0EN7/UbtMtx6M+mdSG64+NpqGiow5
                            |zKVLMO8+8f3UuOyP8X/qLmxgKdeoLuC4AMCg5zl5XSWVskjasPz87wg1kIxah2n2gECW4nTmh0x2
                            |XtsrWaH2HaNTmhj+SLFegZnwSTgGCgSMa13Ob81+Tb2Yx4JCvdVjbEWAWWpug/llVelPIYOlhZbJ
                            |a6pa4zPp1oE1RTrBlt7d/AE1hlBvotNdZpqbxX+kZaNso5Jslj4kV/9Kn2HgjVTSy4tuKZj7xEOH
                            |zVndm/zYGr9mQwZrtLuNSOHndNw6GHvw9M+2ee5uErCAXhsCs1kKlDDI9oCdcavKRpl5cxztl6pe
                            |+MW3+xhm5Q1OB4IgR9wU8bzCGyKpi30tcgIQqZB6GFOw8Qi1w/iE/EeGpMqxCG+NVtRQWSMIwC6X
                            |H1nxKaWd3vK0w3wCpQ/ubsOmdbgZ2kN5IL7yAsk46QFgwpqaYI4Z4IvO5Z4G/VC9ier3+my19vp3
                            |nLFunwTq0S0d9yPN5dmaCwrGSw72jyRHxsL7LoXwhR443wniKg3u5cnbVmdkbZhL/tqWCxpyLuUR
                            |af9eUjDo9OE0DGmovC43DxumdpYg/T3UOx4Fm18xYPOq8PsFxwDiAqw0c+HmRevPl26mYzqYeoDK
                            |m9VeliDupzhO/s1dlD2n9O0i9lQyifM8AWcUuPkSIauQd8P+ELiba4NE6UvBXkRPFrrTOb3mv4dS
                            |BdGideFS6Pstpo/a46jdyEb3rpbKoulKEQFYvPGuLLzRXHGRZ5kU/ZnPXf3VWyQOZy15tlEXDtVO
                            |P5IO+RefH7C8nyy70Y63ylJp9L0YLg1RhfI61p/0xiN0xblvvaWuvt9y/tYaVrwp001ycmQRolNm
                            |hO7mkg1NNY0CSqdynBta6Z7SpYurTDw+hOvcXnHVs9JTV56CG/l6fKwWB7hj9CQKNmm0RdsrH6eV
                            |PzQx4B+IaCXzTlmdX1kvwroWv2EBGIVA3Ov822+VA0+CiIYIo00BS9zHk0yovxGt2wySNgS/NKCp
                            |0XsKBeO8DdwHaPZdKfSfpCIsWE77XS7cYrDAfReq5x/et/8IH5dgtz7HWRMA2Y8ZRrz8XrY7AHpc
                            |KWwzSWu3zp35ogOBuHZNxuRPJfwWLltQgIQsSvrDgOS2zuyUa0j5c+HMcYKrYIzoW3iXViUqnrEW
                            |n6hK30Oks5TO0JZpJawHv2XekOGyrlvi1OW/B1RnrvJni+K/2brOS6rncWkiE50mGsSWsVdTSU6g
                            |ilel+PWyhVO2lISMXTMZGIyY188p++oBV/WnTYLWjI1EqIY4tLd9NqSuIJm0qplRxXIuoGgtAp6D
                            |j2lcKRZZWByrz5ywH94rdf0tZZ6tjN06eMvoc+2e18S3UnPjWRhGCr9kdLfDBZaECGH5sGBLL5QY
                            |BfJggMKfs4sLmUTN8ZUp7GzKXbKxOTri9PJibfT3zFClYnKFlkIPT8sbuJwUAC1ixg8BGrzjlDms
                            |EJCo7Zvo89iFXD3Mx6CF+WWuvmfGhLl6cjrg9EZMT94uxPtwYCzFTx3X4i7pKELJ87/mtNupSdkc
                            |MI77ajXwuOxFSd3kPmc0k0s20721ZF7DShynn9g5LHiV1PGLj4K+90cYL/yINW22vPVHKWVn/1pU
                            |4/Ke0AhErQ/Cb56Yn7NSby+Ks32Wh5eXfryM0xAi6d41+CXGqFHAstgseoda47Fhhp0TaD0ggsFW
                            |7OgsZM227/Q/I/3J2z8occMYsfPti6eSjZjdSwfOun6nnkOjuBLPkHsRSWK1QgoJx+JhJ0pN8B9S
                            |0NdyjcHFE+l6EUWeCLryDEG3xq5bxBKot2MHqgJr/R8mw/0f/hT4izj5ND5rAib3jswu4eMDxO/f
                            |//F9oNT6T6FEHYHpbjGMYF8lxvmZg2pjCF7oqN61v0+Uh7Mb0orObWlAIaQPxLSsTGthkHTRN1Db
                            |NKqEUUObxJrpKv8u2QwfQE4oEysnRfA8n4Ur/7h0Y0nxv5iEHjJpYcGrC1pdHM0CfZguOInn2/7I
                            |o7Ep+C6yZvGw4ZYjbqt/mUK3hiVBmNzQR/Eh/grS7X8+u4Tj6613MAoLbbyoypPj8BM4rCIC0TEn
                            |geMfftuJl1cYCjyVsOvXMv9otiQJLo89anAv1VOaPg0cZWNIv7qMTSHb3MXvEQfNVQaxBvsnlHAu
                            |nkvEfa3ZkclihbSCRlIMvkLlE/wUw8sKq27gs1+KHzQeLzXMCktjCohCSnAYNVzNz0d4HMthMy7r
                            |HUEwhVKAucR5ZmWBm7cp8a1jMHoYHTe5j4aELKJd4WSYmc2UtuTRf9/IdrXgTPa4piPNpJSVP7tz
                            |FEkrglHRlJ1GvycSbjJFFZ+ZmJD4QIvfz2HSVdfCZE4TM6xKyYahBLtewWseKWW85pR456fON0pB
                            |XPdjmXRwpGXFciKgc+ljqFY+tuobdVqYEVQJTKgI2FXGzvm9Mf4xrGYU9c5zVNdbybyVYqL8WQQo
                            |TCLtHeKb5n85tj5tjQGd320oQfBU7zep1cXkLRMtqUTCergjfR+aOZBMZDhSrHoAmN54tB8UBtk5
                            |2tjxwNFkTIyHoO3svYsAOfU6EVyMVhGecw0UV/bpnGXGgSGCX6M+j4VTmoHGPV6EtV8cMJ+ASKZ0
                            |ZRkpF4HmVL/0/ozwCTx7Pn4myupDFKWnKNfn/A5p1Xd8y5eHb6dwHLJhj8/rfPUE+/OgGsKv/11J
                            |n+gaL8ssMKQggDlmvMSwt+cgPIX+Y5YUgm+o8kauPVIH0JWj+xTnCSCrBIk/HStsLdG63BGDVG4S
                            |fqbpAt85zb/vUA+Cf49Il5dHURC6BYgeu+fcKbdhZ5mH18Tkk3TM4ybJF5TX9zmTObkD+YYN74mX
                            |70kJghZ5HgXVI0CMpxl8W4okILvuck8z/7at8EzL8/Oh1PZcSVJiae2s+24MISB/4zwn3Fa0h4Lt
                            |ML3k7qju6TSFZ07bC7U2lwfAHaL4XTQH1uzSx7fwNKMGxdAIb3CIBI/ye4IJ46WuhjI3Oc7AUWMA
                            |MSx5w5HcLa8HBtQZI5g+vb5lPu4zA2AhRpGuvbvQ2r44m4tD+wQdIA+2Vjo4R3RS2ZdwYVPB08Yo
                            |vZVH6A9g0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAOgPYNAovZVHU8HTxtmXcGE4R3RSD7ZWOvsE
                            |HSA4m4tDu9DavkaRrr0zA2AhvmU+7iOYPr0HBtQZkdwtrzEsecPAUWMAMjc5zgAAAAEAAAAA
                            |]
            Monster ->  [str|eHpbl9DtsMG+RO3DhdY4OpUmG/gs/JomHC45qK56u98qv5rI8mm2ueLxyOfpk8oxENQHs5nnx2YJ
                            |CuYYsK7s6YZo3c0Shi+OLvqLx6DaCUGRJ8HJZFU/1IXnuYrHq3v17mGMRiMohROXbALJYEH4YO/s
                            |AIp6s4F4G++2u3pucggnPhv9qf36g3gryOhUmAIYXmwVQPTDutE6a9oZl08HPS9ZNYyEVVtMJNAE
                            |QZHRSjUrnzO0390mRjE34E4BMrNOb4md0qW9F7Of3dM7Kx3eBK1ggnb7HEo3BAdUMjlgwsEx4XtA
                            |qM3zCPtXTul9PLd/IDnadAHPf1Y7wshhUNgxiaXgWKMWeWHyXgnDgTvPkD0Bq000lrVk6r9SwaOL
                            |oO9H6EGR78sKnJHVclOqjS338OhGgZzdSXDjDL/TOknfLYJGyBeuMZkVlyg+huRHE8fh0efWk4bj
                            |qFR8PXXTHxx0feTaUqx1az41ND6fOgZynlRSozYGf+es5cQ4LWUhE/BdfYNY7vs2ykHBbm66JKWK
                            |Oj1VzkpjN3pdBla6yE/sljUGDYdYEyia01hfLxMrLt4kwwr6zsUrpIhqCfiekR4a4Yv7JC0oFpsk
                            |SoyAe9Xo8OnB1PU8mgEe0f3phk50h30BelRX2OmeTwDlDR+zyDJ28gkIEzbBpJsWH2+psvQX/dlA
                            |ZpsQLuyPQ0lptTf/LQoy748NO+JMYcZYurrwKCEyzalvoRm0Xu1m0RjU6GX7UL17KRUYki4PobN0
                            |+fePkjAYPs0UcdxKCpw1JARs68OOd83vTvTHESg5xKVZsdBOAWQczhyQXbt9v08WR4TlPVHTk+X9
                            |UGWoeH00AG7VvepkiaJInxDI2EJMBBvlwVMqTA52aqeq+6Ki81Nyprt0CQ1Q7bcuGkWCjxzeWQ2i
                            |DgLDBdhz3labwfUF58lSwzPXfWiDWJwO9XQmjMfwtbOBGNbVnCHIzRrGPWCM6wt/+V9KI2vLa274
                            |aVQWtgWIGR64D1UDRH2pXqcitmUcOYjgZdbMhbqosPIo1Qa/Q8V939+h1adBRXiKPPo3H3lPJMpu
                            |sUVCrTgHkkBhcyjcOLL2/t/XODoEswlWIlaSkkzidc2x43p0/R9P2JI1/n8iK8+HdqVsPF9k9CFX
                            |5F5cAWoZ6037QjA21fpaBCS0/ZBkCNFO8vMfMkJuH9lu1c1T9+CAh2UdkVnOll7sTWz0rOHiUofc
                            |qz3TrN44TFnlhgw7Q4DWPk976vWH4A+1oA33FrH1uauGx0lT8jKnCLRbKgsqR3J7P5a8ghlEd9VJ
                            |mDUrH3w/zSyQ3D9Ca9tZpwosVtaMcwp2vude2J+qUX7Opd3cyqkrHlPz2LNirErCYnXxoVsnomSv
                            |5wofuAqjFGnUHxu18YzCP9eHME9v39dtfMyWMgY43y9epva0/mMFMI7oxlJGSbInDJuzI0nEUW6r
                            |+HRxORcgBJaYsmKhd5R5eryAtSKZP1ahDXij9+90MhyiX0YH99Fp5wDqsbF0EgOoJ3IZTqVQpQBa
                            |nEqNMFrFEVFnbQV3RgMPG+rWdenOVS8nWvBACPrW8R5BFP/Uz/aWcbdu5tOtX5qDByaRzR9juFMr
                            |W+Hlw6JHWp4PjcM2Bz13r5DqDHfN/brZucOJhJP4cQXIjUetNnJEO7bWDLx3Bp4GwHuzOQiqftTM
                            |udLZ/PZPfX8DJ6Y55fV5Jvg5csAIbrQxmyUn3f+1Bf7C7qY7svtTwzL/WUBNGW7NPhAI5P1g/bAf
                            |UVR8NCeC7OX3d9Hxa6Xv3dU6SFA8KyDkn9dn3s4j+MGDy5CcD8HpiNKjfdr/+SEQUx+fGeAYl/pt
                            |T0cuWzA1lIRMn/dqEgz1k6JYGOFWXmnPaMN8teFyDTrJoBlQwjftv0pfrHny3Qy3qbPCT9c/9rHp
                            |MxHFUU0iJSkG4vMv0n7fydfsweSQoTwZZPzw1v7qLKtza/oLkfOCETde8ZOMzR9qDLggQmrUwYXv
                            |YC7nCjyxEB8qYxhGuUuTseavWmIoqfNB10QAibUzh9tLvm6yVBddMsBZ3fWx8OmeVUwUBSD20L5l
                            |LJnDdNjavY0kBd6a6NKWpr8VYsecXwklW8hXlU3R1u8m9GRAWLMyJPnzAcP6ph3HINvOjXrv/LOM
                            |FIDwQrkSUzL7GWtwslhZnqe+q7n6e+r304YpAzDBefuw0DF7/8Hb9PLoy1WbNa1+9DRVUlU+5HAw
                            |qe1wLCp92SWPi7IgOJd5RTr8Kl2cAK8wTMu5jjvuoznAdN0BgSrTbP5XFQOakS3SG0M/dbfoBZfx
                            |bYpSi1zQ0Nc6FOhcO/U1wVxP+eNnYfYIvjcn5Acd9jBfxD2mIinnnhpqiU0MYZnnJQ2Ld/h96VgQ
                            |Ak+b8Fl0pYz5eYfzXtqg9tW7xiaAzz5ShJDMzK/m13IUAzGAiFg5th2HxZzQzjoHbP+vUVVW9S3M
                            |o58FklVQXHpMEiFeRnAMQZJOOj7bZA5GpIyjHiVq3N8F5ukwZ8hqx+0VIOXUcgehaja9Xk5proSw
                            |8zeNokYTSbthyVj5Vt7a7vbm3P9DKTzOZldBJVMP/dnOyrIbwfqHNY1nO4OH9qffE4ddTAMnsqTF
                            |nEvleBTQuNeXmvaSJMa90QIkJrg3xHQSExoxXkK4weEo/4VG2Kr7VATqYQQ5flCeLY1ghMSbwwJw
                            |N/E3W3eSRm6O1w+HL/+zz8ji/eOPSzWBi9qXmfTAPkXrp1iAm5TmgfUepKSSIY4bcy6BSqT43Wgs
                            |wnetgyaw/5EQojyAv52MYO+UCKdmhEIIFdTYwnqD+FKxU9DHQt0FEs5fztL4hgXREUp3RaZ+Uwjx
                            |BNMqa0hLl+hKMjdYbYo8NRPcFuaIwcUWmMRilJzvqFtdeWcg17+585Jyb7ixjJtFD9yX7JlOm2W0
                            |gzbNF7dRZEsBMDhAXMVFkR2DF/2AqaTSUtdCXJwqp0bVZ8+adTzcjZqc2ehNdMdqJ2DYHZ1892tU
                            |AaUr2BbPxCsWSVmgSwSOPg3+/hjaKxBgp/aG1zHa5Io2iHq4vrDrF6yr8B2wVQ3wppAUPcyX6wv7
                            |w0pyf6dVu9l67NlgNbgtLSlpFufUdjZBdZx5oMUqGAiQbz1JJz42aAJli5RS1VAxWuu5FU4bEqrG
                            |+a0A1pFDe8JnZsOvjpImxStcVyqOVtknoH/aPgF2YTdCqBh7oIrDLoKvBBfI1PMs/HiauB6u9l5D
                            |vYKoLhSl2m50Ebm7P8bOzpvURaWvqbWsxLLIyoQ+Ez23JdEK4apzAIZEX0ZPPE147PNmjFDdR65C
                            |o8d54QOnrl5RrikBcRMNNgpGM5aL9xTi+xjc1ZQW0rkzF2yH6p0bRDWRDv/vVlVj2P1ruvrMFGMp
                            |yO9exEyGsKsxPFkePgN1aIMNth94K98fDZlhDwUkC99JKsu2tpESqx/5a7EZe0dCS01eo/GhZlT0
                            |mTT4YATIf+F6gSdiIuB06+oP/6drhsM6eRvo+MYA04WmnRnCVaKL1NLgQ9j4AAZPjBFpgm3CYGXC
                            |QZGjGYsOCKG7OBTjPoJm9guPzalF34sgm5AfimtgJtpTcemhNNspEMITlEd+NoF3Bhe8dRHAfPTc
                            |eSySk/aOj6cO1DaZBMArqsnnqdv00fqs3RSx+jLFF0LAJr3CFIHc/fTBmZTvrVUf/7kNyuVYCBnO
                            |GRYr+cZHL4Y8ZUnZIO9dbBPcE8Ac2OdqR/5JOLnyQ1ccl04+Dtlh9joTLlliNAhq2Wz+AoqYBa8P
                            |8MOghV3o0ZhmcCYVHzG9tKL8aOv6s/Z6h+2VbulK0Kid4FYu/v6CYBkMg0F5wml3phjecmgouZlv
                            |7FzS9zHsfFNGuPxUCtx92qI14fwr57iEQ+PkeMiAmfTqm9qCwBWiMwbum5lpBWtvhGXGcX4ZsZWO
                            |CZ3LjTpBDm5ddW59mEZrjBfmNZXtfExJ31fo40hB1RvRDnJ7C3AQRUxczErSSwdKwGMUQKaaLUEv
                            |/ewfeay8nVjbTep4qZ6cpk5N5WZzEdGt65/ahhT/9b5gBBGKDYRAwQuqP3LLeR6/q0yKuna79F9/
                            |vErwz4vYDveokbcGT7C8uo2lS1ohFZfXDafBOrfzYQ1d2i0iNijObL5a09DZloMFrTpR8tE9NNIN
                            |HrKsn6YmpjA6B7WuiiS8hw0nmgbhna4lgjk44b/IkG/BUM36MhNfr5s1Z7R8fXhxEDHc1gpgYGJm
                            |8LBgqWBHN5eJ/6NLU3Vph++g7aDUHS2UEZIxNXXZ9Wl0hFjKZW3vhBlq5hdW8iSJSnPrdTUbtilk
                            |4TGpwbpaifxlzQCHsCGlDcfbnA07WV/jISrS5ZVEXXT3EgErpeMJxYsvHNjyU2oAuOS7O9LjCC6g
                            |8EIW/2d94CJ/gkp6zs5xv+BWc/6amaGChFF2TcbjNJB98M1phK0mFfe7v8sb6lBsUl56+TOq6AQ9
                            |BGB3fAF7s5lLTTwS1EingpGGYtdngI9RWBLADFts2BvsEvuAzyHBXmMB/sSdqo78ZvoL4WLdl5Nz
                            |IzmYtV5+ciO5HnTR4PM6S1QbvZwtKeSbBnUXK4R83l5Cy9XSzdxoM4nXnaR0Us9HKv5Tf+6SRYK9
                            |29dZVYIYxSz9n2qc0YOKtTcTs/HndSTFsrFWPBOOa9ZT7WMP9jhqbpeRai2OOBQioO14KTMO1P5l
                            |c63rT+hZB5NExCNJJ8nH+TBi2vCqn/KDYi6rLxxaf86x+T22c0WeqNhqrsHvWC5f1ZmFujTALLSd
                            |py88Ry/tAupVXby25+Xfx5Afus7RfxGmLYZylYwsx6nkGuTTv9QG1+vyZofBKPVjVQX0ikepqkxi
                            |mcAq5LYhz796fBCLZk8gg4Ab79tE25QjEZ1/RDVYdrBsHtRyC1dH7D/WBt8XjCIEmLj9ExJR9mWl
                            |8wGXtF7aRkpqFPURLWPIN+Sq39odbM1SdspzX4Ucw7kYoPDYu8djlNpQ2XKMUsi+BJ+hLTfw7gjv
                            |PHa3tcpa9SoiFbQsDd52XebUGzxf9Wd6N4326iX1C86Or9Laj/zUxbfijx7H38p89J/2RprJLF61
                            |D0aLfP14r06iqPEqSPuTaQs7iRPH9mfKaBjpnx0I2aodSvOx8W0RcF/9tbOvhLOgZjO6GItzOThG
                            |dQgMAjYFFkO9csmnjjQPGKvPDixwFYdy9XddwmMjJd73/a5x5OQZi/XlQd+UQrPhadKWiPvD/zID
                            |/mfegz7P8HWWwhmJ9YRNjNCuZKvGBlhfK7ifZADHj6JLkUBRw3fmEzLjV7MbpyiNevmr7FA754su
                            |O1eYRcwCEC1+bmeJW29JgCJKkqYuvSypFn7Yc9xmiVLSwvKbrNi4HVcj5YllfI7hgY+8tPGMIBuu
                            |SOu0pSXKMR9D8Aaz+4FzSEdesZOZuJpUQT6HDe7ZbKl5MEx0Im2nDjAUKAf/pC6qCZdxLRu2t6CB
                            |yjtAsAkFLLs1WySd4g+LsXl++UQHZMWBzbq8VbtnKDx/Yqq62/2yO7PzNNjdGYwKVmkYjarzgB5T
                            |UFCvWjHSKBRgc0Ko5rN+sL/Ws6UVYtgnlD8tu9OwewiYKE79gD4vKRrzbmrSzhqF73Jta5da+6Ea
                            |Xb6uyHzsWgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMh87FoaXb6ul1r7oe9ybWvSzhqFGvNuaoA+
                            |LymYKE7907B7CJQ/LbsVYtgnv9azpeazfrBgc0KoMdIoFFBQr1rzgB5TaRiNqgAAAAEAAAAA
                            |]
        in x
        |])
