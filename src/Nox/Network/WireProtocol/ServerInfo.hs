{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Nox.Network.WireProtocol.ServerInfo where

import Data.Flags
import Data.Flags.TH
import Data.BitVector

import Data.ByteString hiding (group)
import Data.Serialize

import Data.Yaml
import Control.Applicative (pure)
import Control.Monad (mzero)

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
    [ ("unuSp0",                 1 `shiftL` 0)
    , ("anchor",                 1 `shiftL` 1)
    , ("unuSp2",                 1 `shiftL` 2)
    , ("unuSp3",                 1 `shiftL` 3)
    , ("blink",                  1 `shiftL` 4)
    , ("burn",                   1 `shiftL` 5)
    , ("unuSp6",                 1 `shiftL` 6)
    , ("unuSp7",                 1 `shiftL` 7)
    , ("channelLife",            1 `shiftL` 8)
    , ("charmCreature",          1 `shiftL` 9)
    , ("circleOfFire",           1 `shiftL` 10)
    , ("unuSp11",                1 `shiftL` 11)
    , ("confuse",                1 `shiftL` 12)
    , ("counterspell",           1 `shiftL` 13)
    , ("curePoison",             1 `shiftL` 14)
    , ("unuSp15",                1 `shiftL` 15)
    , ("deathRay",               1 `shiftL` 16)
    , ("unuSp17",                1 `shiftL` 17)
    , ("unuSp18",                1 `shiftL` 18)
    , ("detonateSeenTraps",      1 `shiftL` 19)
    , ("unuSp20",                1 `shiftL` 20)
    , ("dispelUndead",           1 `shiftL` 21)
    , ("drainMana",              1 `shiftL` 22)
    , ("earthquake",             1 `shiftL` 23)
    , ("energyBolt",             1 `shiftL` 24)
    , ("unuSp25",                1 `shiftL` 25)
    , ("fear",                   1 `shiftL` 26)
    , ("fireball",               1 `shiftL` 27)
    , ("unuSp28",                1 `shiftL` 28)
    , ("fistOfVengeance",        1 `shiftL` 29)
    , ("unuSp30",                1 `shiftL` 30)
    , ("forceOfNature",          1 `shiftL` 31)
    , ("unuSp32",                1 `shiftL` 32)
    , ("fumble",                 1 `shiftL` 33)
    , ("trap",                   1 `shiftL` 34)
    , ("greaterHeal",            1 `shiftL` 35)
    , ("haste",                  1 `shiftL` 36)
    , ("infravision",            1 `shiftL` 37)
    , ("inversion",              1 `shiftL` 38)
    , ("invisibility",           1 `shiftL` 39)
    , ("invulnerability",        1 `shiftL` 40)
    , ("lesserHeal",             1 `shiftL` 41)
    , ("light",                  1 `shiftL` 42)
    , ("lightning",              1 `shiftL` 43)
    , ("lock",                   1 `shiftL` 44)
    , ("unuSp45",                1 `shiftL` 45)
    , ("markLocation1",          1 `shiftL` 46)
    , ("markLocation2",          1 `shiftL` 47)
    , ("markLocation3",          1 `shiftL` 48)
    , ("markLocation4",          1 `shiftL` 49)
    , ("misslesOfMagic",         1 `shiftL` 50)
    , ("forceField",             1 `shiftL` 51)
    , ("meteor",                 1 `shiftL` 52)
    , ("unuSp53",                1 `shiftL` 53)
    , ("moonglow",               1 `shiftL` 54)
    , ("unuSp55",                1 `shiftL` 55)
    , ("obliteration",           1 `shiftL` 56)
    , ("unuSp57",                1 `shiftL` 57)
    , ("pixieSwarm",             1 `shiftL` 58)
    , ("unuSp59",                1 `shiftL` 59)
    , ("poison",                 1 `shiftL` 60)
    , ("protectFromShock",       1 `shiftL` 61)
    , ("protectFromFire",        1 `shiftL` 62)
    , ("unuSp63",                1 `shiftL` 63)
    , ("protectFromPoison",      1 `shiftL` 64)
    , ("pull",                   1 `shiftL` 65)
    , ("push",                   1 `shiftL` 66)
    , ("reflectiveShield",       1 `shiftL` 67)
    , ("unuSp68",                1 `shiftL` 68)
    , ("unuSp69",                1 `shiftL` 69)
    , ("run",                    1 `shiftL` 70)
    , ("shock",                  1 `shiftL` 71)
    , ("slow",                   1 `shiftL` 72)
    , ("unuSp73",                1 `shiftL` 73)
    , ("stun",                   1 `shiftL` 74)
    , ("summonBat",              1 `shiftL` 75)
    , ("summonBlackBear",        1 `shiftL` 76)
    , ("summonGrizzlyBear",      1 `shiftL` 77)
    , ("summonBeholder",         1 `shiftL` 78)
    , ("unuSp79",                1 `shiftL` 79)
    , ("summonCarnivorousPlant", 1 `shiftL` 80)
    , ("summonCaveSpiderLarge",  1 `shiftL` 81)
    , ("summonCaveSpiderSmall",  1 `shiftL` 82)
    , ("summonGargoyle",         1 `shiftL` 83)
    , ("summonEmberDemon",       1 `shiftL` 84)
    , ("summonGhost",            1 `shiftL` 85)
    , ("summonGiantLeech",       1 `shiftL` 86)
    , ("summonImp",              1 `shiftL` 87)
    , ("summonMechanicalFlyer",  1 `shiftL` 88)
    , ("summonMechanicalGolem",  1 `shiftL` 89)
    , ("summonMimic",            1 `shiftL` 90)
    , ("summonOgress",           1 `shiftL` 91)
    , ("summonOgre",             1 `shiftL` 92)
    , ("summonOgreLord",         1 `shiftL` 93)
    , ("summonScorpion",         1 `shiftL` 94)
    , ("summonShade",            1 `shiftL` 95)
    , ("summonSkeleton",         1 `shiftL` 96)
    , ("summonSkeletonLord",     1 `shiftL` 97)
    , ("summonSpiderPoisonous",  1 `shiftL` 98)
    , ("summonSpiderSmall",      1 `shiftL` 99)
    , ("summonSpiderSpitting",   1 `shiftL` 100)
    , ("summonStoneGolem",       1 `shiftL` 101)
    , ("summonTroll",            1 `shiftL` 102)
    , ("summonUrchin",           1 `shiftL` 103)
    , ("summonWasp",             1 `shiftL` 104)
    , ("summonWillOWisp",        1 `shiftL` 105)
    , ("summonWolf",             1 `shiftL` 106)
    , ("summonWolfBlack",        1 `shiftL` 107)
    , ("summonWolfWhite",        1 `shiftL` 108)
    , ("summonZombie",           1 `shiftL` 109)
    , ("summonZombieVile",       1 `shiftL` 110)
    , ("summonDemon",            1 `shiftL` 111)
    , ("summonLich",             1 `shiftL` 112)
    , ("summonDryad",            1 `shiftL` 113)
    , ("summonUrchinShaman",     1 `shiftL` 114)
    , ("swapLocation",           1 `shiftL` 115)
    , ("tag",                    1 `shiftL` 116)
    , ("unuSp117",               1 `shiftL` 117)
    , ("unuSp118",               1 `shiftL` 118)
    , ("unuSp119",               1 `shiftL` 119)
    , ("unuSp120",               1 `shiftL` 120)
    , ("unuSp121",               1 `shiftL` 121)
    , ("teleportToMarker1",      1 `shiftL` 122)
    , ("teleportToMarker2",      1 `shiftL` 123)
    , ("teleportToMarker3",      1 `shiftL` 124)
    , ("teleportToMarker4",      1 `shiftL` 125)
    , ("teleportToTarget",       1 `shiftL` 126)
    , ("telekinesis",            1 `shiftL` 127)
    , ("toxicCloud",             1 `shiftL` 128)
    , ("triggerTrap",            1 `shiftL` 129)
    , ("vampirism",              1 `shiftL` 130)
    , ("unuSp131",               1 `shiftL` 131)
    , ("wall",                   1 `shiftL` 132)
    , ("unuSp133",               1 `shiftL` 133)
    , ("summonCreature",         1 `shiftL` 134)
    , ("markLocation",           1 `shiftL` 135)
    , ("teleportToMarker",       1 `shiftL` 136) -- what is this unindexed version? it selects all 4?
    , ("unuSp137",               1 `shiftL` 137)
    , ("unuSp138",               1 `shiftL` 138)
    , ("unuSp139",               1 `shiftL` 139)
    , ("unuSp140",               1 `shiftL` 140)
    , ("unuSp141",               1 `shiftL` 141)
    , ("unuSp142",               1 `shiftL` 142)
    , ("unuSp143",               1 `shiftL` 143)
    ]

bitmaskWrapper "AllowedWeapons" ''BV []
    [ ("flag",                  1 `shiftL` 0)
    , ("quiver",                1 `shiftL` 1)
    , ("bow",                   1 `shiftL` 2)
    , ("crossbow",              1 `shiftL` 3)
    , ("unuWp4",                1 `shiftL` 4)
    , ("unuWp5",                1 `shiftL` 5)
    , ("chakrum",               1 `shiftL` 6)
    , ("shuriken",              1 `shiftL` 7)

    , ("sword",                 1 `shiftL` 8)
    , ("longSword",             1 `shiftL` 9)
    , ("greatSword",            1 `shiftL` 10)
    , ("mace",                  1 `shiftL` 11)
    , ("battleAxe",             1 `shiftL` 12)
    , ("ogreAxe",               1 `shiftL` 13)
    , ("warHammer",             1 `shiftL` 14)
    , ("staff",                 1 `shiftL` 15)

    , ("sulphurousFlareStaff",  1 `shiftL` 16)
    , ("sulphurousShowerStaff", 1 `shiftL` 17)
    , ("lightningStaff",        1 `shiftL` 18)
    , ("fireballStaff",         1 `shiftL` 19)
    , ("tripleFireballStaff",   1 `shiftL` 20)
    , ("forceOfNatureStaff",    1 `shiftL` 21)
    , ("wandOfDeath",           1 `shiftL` 22)
    , ("halberdOfHorrendous",   1 `shiftL` 23)
    ]

bitmaskWrapper "AllowedArmors" ''BV []
    [ ("sneakers",          1 `shiftL` 0)
    , ("cloak",             1 `shiftL` 1)
    , ("pants",             1 `shiftL` 2)
    , ("medievalPants",     1 `shiftL` 3)
    , ("leatherLeggings",   1 `shiftL` 4)
    , ("chainmailLeggings", 1 `shiftL` 5)
    , ("leatherBoots",      1 `shiftL` 6)
    , ("armoredBoots",      1 `shiftL` 7)
    , ("plateBoots",        1 `shiftL` 8)
    , ("plateLeggings",     1 `shiftL` 9)
    , ("shirt",             1 `shiftL` 10)
    , ("medievalShirt",     1 `shiftL` 11)
    , ("leatherArmbands",   1 `shiftL` 12)
    , ("plateArms",         1 `shiftL` 13)
    , ("wizardRobe",        1 `shiftL` 14)
    , ("leatherTunic",      1 `shiftL` 15)
    , ("chainmailTunic",    1 `shiftL` 16)
    , ("breastplate",       1 `shiftL` 17)
    , ("chainCoif",         1 `shiftL` 18)
    , ("wizardHelm",        1 `shiftL` 19)
    , ("conjurerHelm",      1 `shiftL` 20)
    , ("leatherHelm",       1 `shiftL` 21)
    , ("plateHelm",         1 `shiftL` 22)
    , ("knightsHelm",       1 `shiftL` 23)
    , ("roundShield",       1 `shiftL` 24)
    , ("kiteShield",        1 `shiftL` 25)
    , ("unuAr26",           1 `shiftL` 26)
    , ("unuAr27",           1 `shiftL` 27)
    , ("unuAr28",           1 `shiftL` 28)
    , ("unuAr29",           1 `shiftL` 29)
    , ("unuAr30",           1 `shiftL` 30)
    , ("unuAr31",           1 `shiftL` 31)
    ]

-- 0=640res,1=800,2=1024, changes to high6? bits here crashes game
bitmaskWrapper "Resolution" ''BV []
    [ ("resLow",  0)
    , ("resMed",  1  `shiftL` 0)
    , ("resHigh", 1  `shiftL` 1)
    ]

bitmaskWrapper "GameType" ''BV []
    [ ("arena",            1 `shiftL` 1)  -- also just 0 and all other elided bits
    , ("ctf",              1 `shiftL` 4)
    , ("kotr",             1 `shiftL` 5)
    , ("flagball",         1 `shiftL` 6)
    , ("chat",             1 `shiftL` 7)
    , ("elimination",      1 `shiftL` 10)
    , ("quest",            1 `shiftL` 12)
    , ("individualLadder", 1 `shiftL` 14)
    , ("clanLadder",       1 `shiftL` 15)
    ]

-- TODO make singular putBitVectorAsBytes
putAllowedSpells  (AllowedSpells  mask) = putByteString $ pack (Prelude.map (fromIntegral . nat) (Prelude.reverse (group 8 mask)))
putAllowedWeapons (AllowedWeapons mask) = putByteString $ pack (Prelude.map (fromIntegral . nat) (Prelude.reverse (group 8 mask)))
putAllowedArmors  (AllowedArmors  mask) = putByteString $ pack (Prelude.map (fromIntegral . nat) (Prelude.reverse (group 8 mask)))
putGameType       (GameType       mask) = putByteString $ pack (Prelude.map (fromIntegral . nat) (Prelude.reverse (group 8 mask)))
putResolution     (Resolution     mask) = putByteString $ pack (Prelude.map (fromIntegral . nat) (Prelude.reverse (group 8 mask)))


instance FromJSON AllowedSpells where
    -- TODO: TH for these aliases
    parseJSON (String "unuSp0")            = pure unuSp0
    parseJSON (String "anchor")            = pure anchor
    parseJSON (String "unuSp2")            = pure unuSp2
    parseJSON (String "unuSp3")            = pure unuSp3
    parseJSON (String "blink")             = pure blink
    parseJSON (String "burn")              = pure burn
    parseJSON (String "unuSp6")            = pure unuSp6
    parseJSON (String "unuSp7")            = pure unuSp7
    parseJSON (String "channelLife")      = pure channelLife
    parseJSON (String "charmCreature")     = pure charmCreature
    parseJSON (String "circleOfFire")        = pure circleOfFire
    parseJSON (String "unuSp11")           = pure unuSp11
    parseJSON (String "confuse")           = pure confuse
    parseJSON (String "counterspell")      = pure counterspell
    parseJSON (String "curePoison")        = pure curePoison
    parseJSON (String "unuSp15")           = pure unuSp15
    parseJSON (String "deathRay")          = pure deathRay
    parseJSON (String "unuSp17")           = pure unuSp17
    parseJSON (String "unuSp18")           = pure unuSp18
    parseJSON (String "detonateSeenTraps") = pure detonateSeenTraps
    parseJSON (String "unuSp20")           = pure unuSp20
    parseJSON (String "dispelUndead")      = pure dispelUndead
    parseJSON (String "drainMana")         = pure drainMana
    parseJSON (String "earthquake")        = pure earthquake
    parseJSON (String "energyBolt")        = pure energyBolt
    parseJSON (String "unuSp25")           = pure unuSp25
    parseJSON (String "fear")              = pure fear
    parseJSON (String "fireball")          = pure fireball
    parseJSON (String "unuSp28")           = pure unuSp28
    parseJSON (String "fistOfVengeance")           = pure fistOfVengeance
    parseJSON (String "unuSp30")           = pure unuSp30
    parseJSON (String "forceOfNature")           = pure forceOfNature
    parseJSON (String "unuSp32")           = pure unuSp32
    parseJSON (String "fumble")           = pure fumble
    parseJSON (String "trap")           = pure trap
    parseJSON (String "greaterHeal")           = pure greaterHeal
    parseJSON (String "haste")           = pure haste
    parseJSON (String "infravision")           = pure infravision
    parseJSON (String "inversion")           = pure inversion
    parseJSON (String "invisibility")      = pure invisibility
    parseJSON (String "invulnerability")   = pure invulnerability
    parseJSON (String "lesserHeal")        = pure lesserHeal
    parseJSON (String "light")             = pure light
    parseJSON (String "lightning")         = pure lightning
    parseJSON (String "lock")              = pure lock
    parseJSON (String "unuSp45")           = pure unuSp45
    parseJSON (String "markLocation1")     = pure markLocation1
    parseJSON (String "markLocation2")     = pure markLocation2
    parseJSON (String "markLocation3")     = pure markLocation3
    parseJSON (String "markLocation4")     = pure markLocation4
    parseJSON (String "misslesOfMagic")           = pure misslesOfMagic
    parseJSON (String "forceField")           = pure forceField
    parseJSON (String "meteor")           = pure meteor
    parseJSON (String "unuSp53")           = pure unuSp53
    parseJSON (String "moonglow")           = pure moonglow
    parseJSON (String "unuSp55")           = pure unuSp55
    parseJSON (String "obliteration")           = pure obliteration
    parseJSON (String "unuSp57")           = pure unuSp57
    parseJSON (String "pixieSwarm")           = pure pixieSwarm
    parseJSON (String "unuSp59")           = pure unuSp59
    parseJSON (String "poison")           = pure poison
    parseJSON (String "protectFromShock")           = pure protectFromShock
    parseJSON (String "protectFromFire")           = pure protectFromFire
    parseJSON (String "unuSp63")           = pure unuSp63
    parseJSON (String "protectFromPoison")           = pure protectFromPoison
    parseJSON (String "pull")           = pure pull
    parseJSON (String "push")           = pure push
    parseJSON (String "reflectiveShield")           = pure reflectiveShield
    parseJSON (String "unuSp68")           = pure unuSp68
    parseJSON (String "unuSp69")           = pure unuSp69
    parseJSON (String "run")           = pure run
    parseJSON (String "shock")           = pure shock
    parseJSON (String "slow")           = pure slow
    parseJSON (String "unuSp73")           = pure unuSp73
    parseJSON (String "stun")           = pure stun
    parseJSON (String "summonBat")           = pure summonBat
    parseJSON (String "summonBlackBear")           = pure summonBlackBear
    parseJSON (String "summonGrizzlyBear")           = pure summonGrizzlyBear
    parseJSON (String "summonBeholder")           = pure summonBeholder
    parseJSON (String "unuSp79")           = pure unuSp79
    parseJSON (String "summonCarnivorousPlant")           = pure summonCarnivorousPlant
    parseJSON (String "summonCaveSpiderLarge")           = pure summonCaveSpiderLarge
    parseJSON (String "summonCaveSpiderSmall")           = pure summonCaveSpiderSmall
    parseJSON (String "summonGargoyle")           = pure summonGargoyle
    parseJSON (String "summonEmberDemon")           = pure summonEmberDemon
    parseJSON (String "summonGhost")           = pure summonGhost
    parseJSON (String "summonGiantLeech")           = pure summonGiantLeech
    parseJSON (String "summonImp")           = pure summonImp
    parseJSON (String "summonMechanicalFlyer")           = pure summonMechanicalFlyer
    parseJSON (String "summonMechanicalGolem")           = pure summonMechanicalGolem
    parseJSON (String "summonMimic")           = pure summonMimic
    parseJSON (String "summonOgress")           = pure summonOgress
    parseJSON (String "summonOgre")           = pure summonOgre
    parseJSON (String "summonOgreLord")           = pure summonOgreLord
    parseJSON (String "summonScorpion")           = pure summonScorpion
    parseJSON (String "summonShade")           = pure summonShade
    parseJSON (String "summonSkeleton")           = pure summonSkeleton
    parseJSON (String "summonSkeletonLord")           = pure summonSkeletonLord
    parseJSON (String "summonSpiderPoisonous")           = pure summonSpiderPoisonous
    parseJSON (String "summonSpiderSmall")           = pure summonSpiderSmall
    parseJSON (String "summonSpiderSpitting")          = pure summonSpiderSpitting
    parseJSON (String "summonStoneGolem")          = pure summonStoneGolem
    parseJSON (String "summonTroll")          = pure summonTroll
    parseJSON (String "summonUrchin")          = pure summonUrchin
    parseJSON (String "summonWasp")          = pure summonWasp
    parseJSON (String "summonWillOWisp")          = pure summonWillOWisp
    parseJSON (String "summonWolf")          = pure summonWolf
    parseJSON (String "summonWolfBlack")          = pure summonWolfBlack
    parseJSON (String "summonWolfWhite")          = pure summonWolfWhite
    parseJSON (String "summonZombie")          = pure summonZombie
    parseJSON (String "summonZombieVile")          = pure summonZombieVile
    parseJSON (String "summonDemon")          = pure summonDemon
    parseJSON (String "summonLich")          = pure summonLich
    parseJSON (String "summonDryad")          = pure summonDryad
    parseJSON (String "summonUrchinShaman")          = pure summonUrchinShaman
    parseJSON (String "swapLocation")          = pure swapLocation
    parseJSON (String "tag")          = pure tag
    parseJSON (String "unuSp117")          = pure unuSp117
    parseJSON (String "unuSp118")          = pure unuSp118
    parseJSON (String "unuSp119")          = pure unuSp119
    parseJSON (String "unuSp120")          = pure unuSp120
    parseJSON (String "unuSp121")          = pure unuSp121
    parseJSON (String "teleportToMarker1") = pure teleportToMarker1
    parseJSON (String "teleportToMarker2") = pure teleportToMarker2
    parseJSON (String "teleportToMarker3") = pure teleportToMarker3
    parseJSON (String "teleportToMarker4") = pure teleportToMarker4
    parseJSON (String "teleportToTarget")  = pure teleportToTarget
    parseJSON (String "telekinesis")       = pure telekinesis
    parseJSON (String "toxicCloud")        = pure toxicCloud
    parseJSON (String "triggerTrap")       = pure triggerTrap
    parseJSON (String "vampirism")          = pure vampirism
    parseJSON (String "unuSp131")          = pure unuSp131
    parseJSON (String "wall")          = pure wall
    parseJSON (String "unuSp133")          = pure unuSp133
    parseJSON (String "summonCreature")          = pure summonCreature
    parseJSON (String "markLocation")          = pure markLocation
    parseJSON (String "teleportToMarker")  = pure teleportToMarker
    parseJSON (String "unuSp137")          = pure unuSp137
    parseJSON (String "unuSp138")          = pure unuSp137
    parseJSON (String "unuSp139")          = pure unuSp139
    parseJSON (String "unuSp140")          = pure unuSp140
    parseJSON (String "unuSp141")          = pure unuSp141
    parseJSON (String "unuSp142")          = pure unuSp142
    parseJSON (String "unuSp143")          = pure unuSp143
    parseJSON _                    = mzero

instance FromJSON AllowedWeapons where
    parseJSON (String "flag") = pure flag
    parseJSON (String "quiver") = pure quiver
    parseJSON (String "bow") = pure bow
    parseJSON (String "crossbow") = pure crossbow
    parseJSON (String "unuWp4") = pure unuWp4
    parseJSON (String "unuWp5") = pure unuWp5
    parseJSON (String "chakrum") = pure chakrum
    parseJSON (String "shuriken") = pure shuriken
    parseJSON (String "sword") = pure sword
    parseJSON (String "longSword") = pure longSword
    parseJSON (String "greatSword") = pure greatSword
    parseJSON (String "mace") = pure mace
    parseJSON (String "battleAxe") = pure battleAxe
    parseJSON (String "ogreAxe") = pure ogreAxe
    parseJSON (String "warHammer") = pure warHammer
    parseJSON (String "staff") = pure staff
    parseJSON (String "sulphurousFlareStaff") = pure sulphurousFlareStaff
    parseJSON (String "sulphurousShowerStaff") = pure sulphurousShowerStaff
    parseJSON (String "lightningStaff") = pure lightningStaff
    parseJSON (String "fireballStaff") = pure fireballStaff
    parseJSON (String "tripleFireballStaff") = pure tripleFireballStaff
    parseJSON (String "forceOfNatureStaff") = pure forceOfNatureStaff
    parseJSON (String "wandOfDeath") = pure wandOfDeath
    parseJSON (String "halberdOfHorrendous") = pure halberdOfHorrendous
    parseJSON _                    = mzero

instance FromJSON AllowedArmors where
    parseJSON (String "sneakers") = pure sneakers
    parseJSON (String "cloak") = pure cloak
    parseJSON (String "pants") = pure pants
    parseJSON (String "medievalPants") = pure medievalPants
    parseJSON (String "leatherLeggings") = pure leatherLeggings
    parseJSON (String "chainmailLeggings") = pure chainmailLeggings
    parseJSON (String "leatherBoots") = pure leatherBoots
    parseJSON (String "armoredBoots") = pure armoredBoots
    parseJSON (String "plateBoots") = pure plateBoots
    parseJSON (String "plateLeggings") = pure plateLeggings
    parseJSON (String "shirt") = pure shirt
    parseJSON (String "medievalShirt") = pure medievalShirt
    parseJSON (String "leatherArmbands") = pure leatherArmbands
    parseJSON (String "plateArms") = pure plateArms
    parseJSON (String "wizardRobe") = pure wizardRobe
    parseJSON (String "leatherTunic") = pure leatherTunic
    parseJSON (String "chainmailTunic") = pure chainmailTunic
    parseJSON (String "breastplate") = pure breastplate
    parseJSON (String "chainCoif") = pure chainCoif
    parseJSON (String "wizardHelm") = pure wizardHelm
    parseJSON (String "conjurerHelm") = pure conjurerHelm
    parseJSON (String "leatherHelm") = pure leatherHelm
    parseJSON (String "plateHelm") = pure plateHelm
    parseJSON (String "knightsHelm") = pure knightsHelm
    parseJSON (String "roundShield") = pure roundShield
    parseJSON (String "kiteShield") = pure kiteShield
    parseJSON (String "unuAr26") = pure unuAr26
    parseJSON (String "unuAr27") = pure unuAr27
    parseJSON (String "unuAr28") = pure unuAr28
    parseJSON (String "unuAr29") = pure unuAr29
    parseJSON (String "unuAr30") = pure unuAr30
    parseJSON (String "unuAr31") = pure unuAr31
    parseJSON _                    = mzero

