{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Nox.Network.WireProtocol.ServerInfo where

import Data.Flags
import Data.Flags.TH
import Data.BitVector

import Data.ByteString hiding (group)
import Data.Serialize

instance Flags BV where
    noFlags = bitVec 0 0
    andFlags = (.|.)
    -- Writing just "(complement b)" here will prefix the value with 0 bits
    -- before being anded, resulting in a truncated mask (e.g. [4]15 .&.
    -- (complement [1]1) = [4]0 ), so we need to reconstruct each side,
    -- ensuring equal size of each vector.
    butFlags a b = bitVec n a .&. (complement $ bitVec n b)
        where n = max (size a) (size b)
    commonFlags = (.&.)

-- TODO these will be the ids used later on in packets, not the okSpells field
data Spell = Anchor
           | Blink
           | Burn
           deriving (Eq, Show, Enum)

-- TODO: I would prefer something that is unified. this solution is limited to 64bit length bitmasks
-- The only way to do that, I think, is to write a Flags instance for ByteString and use that as the wrapped type
-- TODO: populate this semi-automatically by having this server return 1 successive bit flipped every ping to work out what
-- each bit and field do.
-- TODO: also probably need to twiddle some settings on a listenserver and look at the returned pong; ideally,
-- use code in here to interrogate the server and dump the current interpretation of the packet.
bitmaskWrapper "AllowedSpells" ''BV []
    [ ("unuSp0",                   1 `shiftL` 0)
    , ("anchor",                   1 `shiftL` 1)
    , ("unuSp2",                   1 `shiftL` 2)
    , ("unuSp3",                   1 `shiftL` 3)
    , ("blink",                    1 `shiftL` 4)
    , ("burn",                     1 `shiftL` 5)
    , ("unuSp6",                   1 `shiftL` 6)
    , ("unuSp7",                   1 `shiftL` 7)

    , ("channelLife",              1 `shiftL` 8)
    , ("charmCreature",            1 `shiftL` 9)
    , ("circleOfFire",             1 `shiftL` 10)
    , ("unuSp11",                  1 `shiftL` 11)
    , ("confuse",                  1 `shiftL` 12)
    , ("counterspell",             1 `shiftL` 13)
    , ("curePoison",               1 `shiftL` 14)
    , ("unuSp15",                  1 `shiftL` 15)

    , ("deathRay",                 1 `shiftL` 16)
    , ("unuSp17",                  1 `shiftL` 17)
    , ("unuSp18",                  1 `shiftL` 18)
    , ("detonateSeenTraps",        1 `shiftL` 19)
    , ("unuSp20",                  1 `shiftL` 20)
    , ("dispelUndead",             1 `shiftL` 21)
    , ("drainMana",                1 `shiftL` 22)
    , ("earthquake",               1 `shiftL` 23)

    , ("energyBolt",               1 `shiftL` 24)
    , ("unkSp25",                  1 `shiftL` 25)
    , ("fear",                     1 `shiftL` 26)
    , ("fireball",                 1 `shiftL` 27)
    , ("unuSp28",                  1 `shiftL` 28)
    , ("fistOfVengeance",          1 `shiftL` 29)
    , ("unuSp30",                  1 `shiftL` 30)
    , ("forceOfNature",            1 `shiftL` 31)

    , ("unuSp32",                  1 `shiftL` 32)
    , ("fumble",                   1 `shiftL` 33)
    , ("trap",                     1 `shiftL` 34)
    , ("greaterHeal",              1 `shiftL` 35)
    , ("haste",                    1 `shiftL` 36)
    , ("infravision",              1 `shiftL` 37)
    , ("inversion",                1 `shiftL` 38)
    , ("invisibility",             1 `shiftL` 39)

    , ("invulnerability",          1 `shiftL` 40)
    , ("lesserHeal",               1 `shiftL` 41)
    , ("light",                    1 `shiftL` 42)
    , ("lightning",                1 `shiftL` 43)
    , ("lock",                     1 `shiftL` 44)
    , ("unuSp45",                  1 `shiftL` 45)
    , ("markLocation1",            1 `shiftL` 46)
    , ("markLocation2",            1 `shiftL` 47)

    , ("markLocation3",            1 `shiftL` 48)
    , ("markLocation4",            1 `shiftL` 49)
    , ("magicMissles",             1 `shiftL` 50)  -- aka misslesOfMagic
    , ("magicShield",              1 `shiftL` 51)  -- aka forceField
    , ("meteor",                   1 `shiftL` 52)
    , ("unuSp53",                  1 `shiftL` 53)
    , ("moonglow",                 1 `shiftL` 54)
    , ("unuSp55",                  1 `shiftL` 55)

    , ("obliteration",             1 `shiftL` 56)
    , ("unuSp57",                  1 `shiftL` 57)
    , ("pixieSwarm",               1 `shiftL` 58)
    , ("unuSp59",                  1 `shiftL` 59)
    , ("poison",                   1 `shiftL` 60)
    , ("protectFromShock",         1 `shiftL` 61)
    , ("protectFromFire",          1 `shiftL` 62)
    , ("unuSp63",                  1 `shiftL` 63)

    , ("protectFromPoison",        1 `shiftL` 64)
    , ("pull",                     1 `shiftL` 65)
    , ("push",                     1 `shiftL` 66)
    , ("reflectiveShield",         1 `shiftL` 67)
    , ("unuSp68",                  1 `shiftL` 68)
    , ("unuSp69",                  1 `shiftL` 69)
    , ("run",                      1 `shiftL` 70)
    , ("shock",                    1 `shiftL` 71)
    , ("slow",                     1 `shiftL` 72)
    , ("unuSp73",                  1 `shiftL` 73)
    , ("stun",                     1 `shiftL` 74)
    , ("summonBat",                1 `shiftL` 75)
    , ("summonBlackBear",          1 `shiftL` 76)
    , ("summonGrizzlyBear",        1 `shiftL` 77)
    , ("summonBeholder",           1 `shiftL` 78)
    , ("unuSp79",                  1 `shiftL` 79)
    , ("summonCarnivorousPlant",   1 `shiftL` 80)
    , ("summonCaveSpiderLarge",    1 `shiftL` 81)
    , ("summonCaveSpiderSmall",    1 `shiftL` 82)
    , ("summonCaveSpiderSpitting", 1 `shiftL` 83)
    , ("summonStoneGolem",         1 `shiftL` 84)
    , ("summonTroll",              1 `shiftL` 85)
    , ("summonUrchin",             1 `shiftL` 86)
    , ("summonWasp",               1 `shiftL` 87)
    , ("summonMechanicalFlyer",    1 `shiftL` 88)
    , ("summonMechanicalGolem",    1 `shiftL` 89)
    , ("summonMimic",              1 `shiftL` 90)
    , ("summonOgress",             1 `shiftL` 91)
    , ("summonOgre",               1 `shiftL` 92)
    , ("summonOgreLord",           1 `shiftL` 93)
    , ("summonScorpion",           1 `shiftL` 94)
    , ("summonShade",              1 `shiftL` 95)
    , ("summonSkeleton",           1 `shiftL` 96)
    , ("summonSkeletonLord",       1 `shiftL` 97)
    , ("summonSpiderPoisonous",    1 `shiftL` 98)
    , ("summonSpiderSmall",        1 `shiftL` 99)
    , ("summonGargoyle",           1 `shiftL` 100)
    , ("summonEmberDemon",         1 `shiftL` 101)
    , ("summonGhost",              1 `shiftL` 102)
    , ("summonGiantLeech",         1 `shiftL` 103)
    , ("summonImp",                1 `shiftL` 104)
    , ("summonWillOWisp",          1 `shiftL` 105)
    , ("summonWolf",               1 `shiftL` 106)
    , ("summonWolfBlack",          1 `shiftL` 107)
    , ("summonWolfWhite",          1 `shiftL` 108)
    , ("summonZombie",             1 `shiftL` 109)
    , ("summonZombieVile",         1 `shiftL` 110)
    , ("summonDemon",              1 `shiftL` 111)
    , ("summonLich",               1 `shiftL` 112)
    , ("summonDryad",              1 `shiftL` 113)
    , ("summonUrchinShaman",       1 `shiftL` 114)
    , ("swapLocation",             1 `shiftL` 115)
    , ("tag",                      1 `shiftL` 116)
    , ("unuSp117",                 1 `shiftL` 117)
    , ("unuSp118",                 1 `shiftL` 118)
    , ("unuSp119",                 1 `shiftL` 119)
    , ("unuSp120",                 1 `shiftL` 120)
    , ("unuSp121",                 1 `shiftL` 121)
    , ("teleportToMarker1",        1 `shiftL` 122)
    , ("teleportToMarker2",        1 `shiftL` 123)
    , ("teleportToMarker3",        1 `shiftL` 124)
    , ("teleportToMarker4",        1 `shiftL` 125)
    , ("teleportToTarget",         1 `shiftL` 126)
    , ("telekinesis",              1 `shiftL` 127)
    , ("toxicCloud",               1 `shiftL` 128)
    , ("triggerTrap",              1 `shiftL` 129)
    , ("vampirism",                1 `shiftL` 130)
    , ("unuSp131",                 1 `shiftL` 131)
    , ("wall",                     1 `shiftL` 132)
    , ("unuSp133",                 1 `shiftL` 133)
    , ("summonCreature",           1 `shiftL` 134) -- all summons?
    , ("markLocation",             1 `shiftL` 135)
    , ("teleportToMarker",         1 `shiftL` 136) -- what is this unnumbered version? it bans all 4?
    , ("unuSp137",                 1 `shiftL` 137)
    , ("unuSp138",                 1 `shiftL` 138)
    , ("unuSp139",                 1 `shiftL` 139)
    , ("unuSp140",                 1 `shiftL` 140)
    , ("unuSp141",                 1 `shiftL` 141)
    , ("unuSp142",                 1 `shiftL` 142)
    , ("unuSp143",                 1 `shiftL` 143)
    ]

bitmaskWrapper "AllowedWeapons" ''BV []
    [  ("flag",          1   `shiftL` 0)
    ,  ("quiver",          1   `shiftL` 1)
    ,  ("bow",          1   `shiftL` 2)
    ,  ("crossbow",          1   `shiftL` 3)
    ,  ("unkWp4",           1   `shiftL` 4) --nothing
    ,  ("unkWp5",            1   `shiftL` 5) --nothing
    ,  ("chakrum",          1   `shiftL` 6)
    ,  ("shuriken",          1   `shiftL` 7)

    ,  ("sword",     1   `shiftL` 8)
    ,  ("longSword",   1   `shiftL` 9)
    ,  ("greatSword",      1   `shiftL` 10)
    ,  ("mace",         1   `shiftL` 11)
    ,  ("battleAxe",         1   `shiftL` 12)
    ,  ("ogreAxe",         1   `shiftL` 13)
    ,  ("warHammer",         1   `shiftL` 14)
    ,  ("staff",         1   `shiftL` 15)

    ,  ("sulphurousFlareStaff",         1   `shiftL` 16)
    ,  ("sulphurousShowerStaff",         1   `shiftL` 17)
    ,  ("lightningStaff",         1   `shiftL` 18)
    ,  ("fireballStaff",         1   `shiftL` 19)
    ,  ("tripleFireballStaff",         1   `shiftL` 20)
    ,  ("forceOfNatureStaff",         1   `shiftL` 21)
    ,  ("wandOfDeath",         1   `shiftL` 22)
    ,  ("halberdOfHorrendous",      1   `shiftL` 23)
    -- 2 other versions of halberd missing!
    ]

bitmaskWrapper "AllowedArmors" ''BV []
    [  ("sneakers",          1   `shiftL` 0)
    ,  ("cloak",          1   `shiftL` 1)
    ,  ("pants",          1   `shiftL` 2)
    ,  ("medievalPants",          1   `shiftL` 3)
    ,  ("leatherLeggings",           1   `shiftL` 4)
    ,  ("chainmailLeggings",            1   `shiftL` 5)
    ,  ("leatherBoots",          1   `shiftL` 6)
    ,  ("armoredBoots",          1   `shiftL` 7)

    ,  ("plateBoots",     1   `shiftL` 8)
    ,  ("plateLeggings",   1   `shiftL` 9)
    ,  ("shirt",      1   `shiftL` 10)
    ,  ("medievalShirt",         1   `shiftL` 11)
    ,  ("leatherArmbands",         1   `shiftL` 12)
    ,  ("plateArms",         1   `shiftL` 13)
    ,  ("wizardRobe",         1   `shiftL` 14)
    ,  ("leatherTunic",         1   `shiftL` 15)

    ,  ("chainmailTunic",         1   `shiftL` 16)
    ,  ("breastplate",         1   `shiftL` 17)
    ,  ("chainCoif",         1   `shiftL` 18)
    ,  ("wizardHelm",         1   `shiftL` 19)
    ,  ("conjurerHelm",         1   `shiftL` 20)
    ,  ("leatherHelm",         1   `shiftL` 21)
    ,  ("plateHelm",         1   `shiftL` 22)
    ,  ("knightsHelm",      1   `shiftL` 23)

    ,  ("roundShield",         1   `shiftL` 24)
    ,  ("kiteShield",         1   `shiftL` 25)
    ,  ("unkAr26",         1   `shiftL` 26) --nothing?
    ,  ("unkAr27",         1   `shiftL` 27) --nothing?
    ,  ("unkAr28",         1   `shiftL` 28) --nothing?
    ,  ("unkAr29",         1   `shiftL` 29) --nothing?
    ,  ("unkAr30",         1   `shiftL` 30) --nothing?
    ,  ("unkAr31",      1   `shiftL` 31) --nothing?
    ]

putAllowedSpells (AllowedSpells mask) = putByteString $ pack (Prelude.map (fromIntegral . nat) (Prelude.reverse (group 8 mask)))
putAllowedWeapons (AllowedWeapons mask) = putByteString $ pack (Prelude.map (fromIntegral . nat) (Prelude.reverse (group 8 mask)))
putAllowedArmors (AllowedArmors mask) = putByteString $ pack (Prelude.map (fromIntegral . nat) (Prelude.reverse (group 8 mask)))


