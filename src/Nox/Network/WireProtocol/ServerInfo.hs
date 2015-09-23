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
    [  ("unkSp0",          1   `shiftL` 0) --nothing
    ,  ("anchor",          1   `shiftL` 1)
    ,  ("unkSp2",          1   `shiftL` 2)
    ,  ("unkSp3",          1   `shiftL` 3)
    ,  ("blink",           1   `shiftL` 4)
    ,  ("burn",            1   `shiftL` 5)
    ,  ("unkSp6",          1   `shiftL` 6)
    ,  ("unkSp7",          1   `shiftL` 7)

    ,  ("channelLife",     1   `shiftL` 8)
    ,  ("charmCreature",   1   `shiftL` 9)
    ,  ("ringOfFire",      1   `shiftL` 10) -- prly "cirleOfFire" for ordering
    ,  ("unkSp11",         1   `shiftL` 11) --nothing
    ,  ("confuse",         1   `shiftL` 12)
    ,  ("counterspell",         1   `shiftL` 13)
    ,  ("curePoison",         1   `shiftL` 14)
    ,  ("unkSp15",         1   `shiftL` 15) --nothing?

    ,  ("deathRay",         1   `shiftL` 16) --nothing
    ,  ("unkSp17",         1   `shiftL` 17) --nothing
    ,  ("unkSp18",         1   `shiftL` 18) --nothing
    ,  ("detonateSeenTraps",         1   `shiftL` 19)
    ,  ("unkSp20",         1   `shiftL` 20) --nothing
    ,  ("dispelUndead",         1   `shiftL` 21)
    ,  ("drainMana",         1   `shiftL` 22)
    ,  ("earthquake",      1   `shiftL` 23)

    ,  ("unkSp24",         1   `shiftL` 24)
    ,  ("unkSp25",         1   `shiftL` 25)
    ,  ("unkSp26",         1   `shiftL` 26)
    ,  ("fireball",        1   `shiftL` 27)
    ,  ("unkSp28",         1   `shiftL` 28)
    ,  ("unkSp29",         1   `shiftL` 29)
    ,  ("unkSp30",         1   `shiftL` 30)
    ,  ("unkSp31",         1   `shiftL` 31)

    ,  ("unkSp32",         1   `shiftL` 32)
    ,  ("unkSp33",         1   `shiftL` 33)
    ,  ("unkSp34",         1   `shiftL` 34)
    ,  ("unkSp35",         1   `shiftL` 35)
    ,  ("unkSp36",         1   `shiftL` 36)
    ,  ("unkSp37",         1   `shiftL` 37)
    ,  ("unkSp38",         1   `shiftL` 38)
    ,  ("invisibility",    1   `shiftL` 39)

    ,  ("invulnerability", 1   `shiftL` 40)
    ,  ("lesserHeal",      1   `shiftL` 41)
    ,  ("light",           1   `shiftL` 42)
    ,  ("lightning",       1   `shiftL` 43)
    ,  ("lock",            1   `shiftL` 44)
    ,  ("unkSp45",         1   `shiftL` 45)
    ,  ("markLocation1",         1   `shiftL` 46)
    ,  ("markLocation2",         1   `shiftL` 47)

    ,  ("markLocation3",   1   `shiftL` 48)
    ,  ("markLocation4",         1   `shiftL` 49)
    ,  ("unkSp50",         1   `shiftL` 50)
    ,  ("unkSp51",         1   `shiftL` 51)
    ,  ("unkSp52",         1   `shiftL` 52)
    ,  ("unkSp53",         1   `shiftL` 53)
    ,  ("unkSp54",         1   `shiftL` 54)
    ,  ("unkSp55",         1   `shiftL` 55)

    ,  ("unkSp56",         1   `shiftL` 56)
    ,  ("unkSp57",         1   `shiftL` 57)
    ,  ("unkSp58",         1   `shiftL` 58)
    ,  ("unkSp59",         1   `shiftL` 59)
    ,  ("unkSp60",         1   `shiftL` 60)
    ,  ("unkSp61",         1   `shiftL` 61)
    ,  ("unkSp62",         1   `shiftL` 62)
    ,  ("unkSp63",         1   `shiftL` 63)

    ,  ("unkSp64",         1   `shiftL` 64)
    ,  ("unkSp65",         1   `shiftL` 65)
    ,  ("unkSp66",         1   `shiftL` 66)
    ,  ("unkSp67",         1   `shiftL` 67)
    ,  ("unkSp68",         1   `shiftL` 68)
    ,  ("unkSp69",         1   `shiftL` 69)
    ,  ("unkSp70",         1   `shiftL` 70)
    ,  ("unkSp71",         1   `shiftL` 71)
    ,  ("unkSp72",         1   `shiftL` 72)
    ,  ("unkSp73",         1   `shiftL` 73)
    ,  ("unkSp74",         1   `shiftL` 74)
    ,  ("unkSp75",         1   `shiftL` 75)
    ,  ("unkSp76",         1   `shiftL` 76)
    ,  ("unkSp77",         1   `shiftL` 77)
    ,  ("unkSp78",         1   `shiftL` 78)
    ,  ("unkSp79",         1   `shiftL` 79)
    ,  ("unkSp80",         1   `shiftL` 80)
    ,  ("unkSp81",         1   `shiftL` 81)
    ,  ("unkSp82",         1   `shiftL` 82)
    ,  ("unkSp83",         1   `shiftL` 83)
    ,  ("unkSp84",         1   `shiftL` 84)
    ,  ("unkSp85",         1   `shiftL` 85)
    ,  ("unkSp86",         1   `shiftL` 86)
    ,  ("unkSp87",         1   `shiftL` 87)
    ,  ("unkSp88",         1   `shiftL` 88)
    ,  ("unkSp89",         1   `shiftL` 89)
    ,  ("unkSp90",         1   `shiftL` 90)
    ,  ("unkSp91",         1   `shiftL` 91)
    ,  ("unkSp92",         1   `shiftL` 92)
    ,  ("unkSp93",         1   `shiftL` 93)
    ,  ("unkSp94",         1   `shiftL` 94)
    ,  ("unkSp95",         1   `shiftL` 95)
    ,  ("unkSp96",         1   `shiftL` 96)
    ,  ("unkSp97",         1   `shiftL` 97)
    ,  ("unkSp98",         1   `shiftL` 98)
    ,  ("unkSp99",         1   `shiftL` 99)
    ,  ("unkSp100",         1   `shiftL` 100)
    ,  ("unkSp101",         1   `shiftL` 101)
    ,  ("unkSp102",         1   `shiftL` 102)
    ,  ("unkSp103",         1   `shiftL` 103)
    ,  ("unkSp104",         1   `shiftL` 104)
    ,  ("unkSp105",         1   `shiftL` 105)
    ,  ("unkSp106",         1   `shiftL` 106)
    ,  ("unkSp107",         1   `shiftL` 107)
    ,  ("unkSp108",         1   `shiftL` 108)
    ,  ("unkSp109",         1   `shiftL` 109)
    ,  ("unkSp110",         1   `shiftL` 110)
    ,  ("unkSp111",         1   `shiftL` 111)
    ,  ("unkSp112",         1   `shiftL` 112)
    ,  ("unkSp113",         1   `shiftL` 113)
    ,  ("unkSp114",         1   `shiftL` 114)
    ,  ("unkSp115",         1   `shiftL` 115)
    ,  ("unkSp116",         1   `shiftL` 116)
    ,  ("unkSp117",         1   `shiftL` 117)
    ,  ("unkSp118",         1   `shiftL` 118)
    ,  ("unkSp119",         1   `shiftL` 119)
    ,  ("unkSp120",         1   `shiftL` 120) --nothing?
    ,  ("unkSp121",         1   `shiftL` 121) --nothing?
    ,  ("teleportToMarker1",         1   `shiftL` 122)
    ,  ("teleportToMarker2",         1   `shiftL` 123)
    ,  ("teleportToMarker3",         1   `shiftL` 124)
    ,  ("teleportToMarker4",         1   `shiftL` 125)
    ,  ("teleportToTarget",         1   `shiftL` 126)
    ,  ("telekinesis",         1   `shiftL` 127)
    ,  ("toxicCloud",         1   `shiftL` 128)
    ,  ("triggerTrap",         1   `shiftL` 129)
    ,  ("unkSp130",         1   `shiftL` 130)
    ,  ("unkSp131",         1   `shiftL` 131)
    ,  ("unkSp132",         1   `shiftL` 132)
    ,  ("unkSp133",         1   `shiftL` 133)
    ,  ("unkSp134",         1   `shiftL` 134)
    ,  ("unkSp135",         1   `shiftL` 135)
    ,  ("teleportToMarker",         1   `shiftL` 136) -- what is this unnumbered version? it bans all 4?
    ,  ("unkSp137",         1   `shiftL` 137) --nothing?
    ,  ("unkSp138",         1   `shiftL` 138) --nothing?
    ,  ("unkSp139",         1   `shiftL` 139) --nothing?
    ,  ("unkSp140",         1   `shiftL` 140) -- nothing?
    ,  ("unkSp141",         1   `shiftL` 141)
    ,  ("unkSp142",         1   `shiftL` 142)
    ,  ("unkSp143",         1   `shiftL` 143) --nothing?
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


