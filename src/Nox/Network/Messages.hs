{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE DataKinds #-}

module Nox.Network.Messages where

import Data.Word
import Data.ByteString
import Data.Serialize

import Nox.Network.WireProtocol hiding (Event(..))
import qualified Nox.Network.WireProtocol as E (Event(..))
import Nox.Game.Player

import Data.Flags
import Data.Flags.TH
import Data.Bits
import Data.Int
import Data.BitVector

instance Flags BV where
    noFlags = bitVec 64 0
    andFlags = (.|.)
    -- need noFlags here since bitVector will be of length equal to value's position, otherwise.
    butFlags a b = a .&. complement (noFlags .|. b)
    commonFlags = (.&.)

instance BoundedFlags BV where
    allFlags = complement noFlags
    enumFlags = undefined


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
    [  ("anchor",          1   `shiftL` 1)
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
    ,  ("unkSp15",         1   `shiftL` 15)

    ,  ("unkSp16",         1   `shiftL` 16) --nothing
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
    ,  ("unkSp46",         1   `shiftL` 46)
    ,  ("unkSp47",         1   `shiftL` 47)

    ,  ("markLocation3",   1   `shiftL` 48)
    ,  ("unkSp49",         1   `shiftL` 49)
    ,  ("unkSp50",         1   `shiftL` 50)
    ,  ("unkSp51",         1   `shiftL` 51)
    ,  ("unkSp52",         1   `shiftL` 52)
    ,  ("unkSp53",         1   `shiftL` 53)
    ,  ("unkSp54",         1   `shiftL` 54)
    ,  ("unkSp55",         1   `shiftL` 55)

    -- TODO: not even sure if this is part of the masks but is usually 0xFF anyway
    ,  ("unkSp56",         1   `shiftL` 56)
    ,  ("unkSp57",         1   `shiftL` 57)
    ,  ("unkSp58",         1   `shiftL` 58)
    ,  ("unkSp59",         1   `shiftL` 59)
    ,  ("unkSp60",         1   `shiftL` 60)
    ,  ("unkSp61",         1   `shiftL` 61)
    ,  ("unkSp62",         1   `shiftL` 62)
    ,  ("unkSp63",         1   `shiftL` 63)
    ]

--test = enumFlags burn 

--type AllowedSpells = Word64
putAllowedSpells (AllowedSpells mask)= putWord64le . fromIntegral $ mask

-- REMEMBER to keep these in enum order else resorting them in 4 spotsgets difficult...
data Message = Event00
             | Event01
             | Event02
             | Event03
             | Event04
             | Event05
             | Event06
             | Event07
             | Event08
             | Event09
             | PlayerDisconnect
             | Event0b
             | PingServer              { unkB1 :: Word8
                                       , unkB2 :: Word8
                                       , unkB3 :: Word8
                                       , unkB4 :: Word8
                                       , unkB5 :: Word8
                                       , timestamp :: Timestamp }
             | PongClient              { actPlayers :: Word8
                                       , maxPlayers :: Word8
                                       , unkB1 :: Word8
                                       , unkB2 :: Word8
                                       , unkB3 :: Word8
                                       , unkB4 :: Word8
                                       , unkB5 :: Word8
                                       , mapName :: ByteString -- 9 long (null term?)
                                       , unkBS1 :: ByteString --  25 of gibberish
                                       , timestamp :: Timestamp
                                       , okSpells :: AllowedSpells
                                       , unkBS2 :: ByteString --  23 bytes of spell allowances, etc.
                                       , gameName :: ByteString } -- null terminated
             | PlayerConnect           { nullB1 :: Word8
                                       , playerName :: ByteString -- 12 wide chars (24)
                                       , unkBS1 :: ByteString -- 28 unknown
                                       , serial :: ByteString -- 22 bytes
                                       , nullS2 :: Word16
                                       , unkL1 :: Word32
                                       , nullL2 :: Word32
                                       , userName :: ByteString -- 8 bytes
                                       , unkS2 :: Word16 }
             | Event0f
             | Event10
             | Event11
             | Event12
             | Event13
             | Event14
             | Event15
             | Event16
             | Event17
             | Event18
             | Event19
             | Event1a
             | Event1b
             | Event1c
             | Event1d
             | Event1e
             | PlayerJoin
             | Event20
             | Event21
             | Event22
             | Event23
             | Event24
             | Event25
             | Event26
	     | PartialTimestamp               { shortTimestamp :: Word16 }
	     | FullTimestamp
	     | NeedTimestamp
	     | SimulatedTimestamp
	     | UseMap
	     | JoinData
             | NewPlayer            { extent      :: Extent
                                    , info        :: ByteString }
	     | PlayerQuit
	     | SimpleObj
	     | ComplexObj
	     | DestroyObject
	     | ObjectOutOfSight
	     | ObjectInShadows
	     | ObjectFriendAdd
	     | ObjectFriendRemove
	     | ResetFriends
	     | EnableObject
	     | DisableObject
	     | DrawFrame
	     | DestroyWall
	     | OpenWall
	     | CloseWall
	     | ChangeOrAddWallMagic
	     | RemoveWallMagic
	     | PlayerInput
	     | PlayerSetWaypoint
	     | ReportHealth         { extent      :: Extent
                                    , health      :: Word16 }
	     | ReportHealthDelta
	     | ReportPlayerHealth
	     | ReportItemHealth
	     | ReportMana           { extent      :: Extent
                                    , mana        :: Word16 }
	     | ReportPoison
	     | ReportStamina
	     | ReportStats                    { extent   :: Extent
                                              , hp       :: HealthPoints
                                              , mp       :: ManaPoints
                                              , weight   :: Weight
                                              , speed    :: Speed
                                              , strength :: Strength
                                              , level    :: Level }
	     | ReportArmorValue
	     | ReportGold
	     | ReportPickup
	     | ReportModifiablePickup
	     | ReportDrop
	     | ReportLesson
	     | ReportMundaneArmorEquip
	     | ReportMundaneWeaponEquip
	     | ReportModifiableWeaponEquip
	     | ReportModifiableArmorEquip
	     | ReportArmorDequip
	     | ReportWeaponDequip
	     | ReportTreasureCount
	     | ReportFlagBallWinner
	     | ReportFlagWinner
	     | ReportDeathmatchWinner
	     | ReportDeathmatchTeamWinner
	     | ReportEnchantment
	     | ReportItemEnchantment
	     | ReportLightColor
	     | ReportLightIntensity
	     | ReportZPlus
	     | ReportZMinus
	     | ReportEquip
	     | ReportDequip
	     | ReportAcquireSpell
	     | ReportTarget
	     | ReportCharges
	     | ReportXStatus
	     | ReportPlayerStatus
	     | ReportModifier
	     | ReportStatModifier
	     | ReportNpc
	     | ReportClientStatus             { extent      :: Extent
                                              , isObserving :: Bool } --Word32
	     | ReportAnimationFrame
	     | ReportAcquireCreature
	     | ReportLoseCreature
	     | ReportExperience
	     | ReportSpellAward
	     | ReportSpellStart
	     | ReportInventoryLoaded
	     | TryDrop
	     | TryGet
	     | TryUse
	     | TryEquip
	     | TryDequip
	     | TryTarget
	     | TryCreatureEvent
	     | TrySpell
	     | TryAbility
	     | TryCollide
	     | FxParticlefx
	     | FxPlasma
	     | FxSummon
	     | FxSummonCancel
	     | FxShield
	     | FxBlueSparks
	     | FxYellowSparks
	     | FxCyanSparks
	     | FxVioletSparks
	     | FxExplosion
	     | FxLesserExplosion
	     | FxCounterspellExplosion
	     | FxThinExplosion
	     | FxTeleport
	     | FxSmokeBlast
	     | FxDamagePoof
	     | FxLightning
	     | FxEnergyBolt
	     | FxChainLightningBolt
	     | FxDrainMana
	     | FxCharm
	     | FxGreaterHeal
	     | FxMagic
	     | FxSparkExplosion
	     | FxDeathRay
	     | FxSentryRay
	     | FxRicochet
	     | FxJiggle
	     | FxGreenBolt
	     | FxGreenExplosion
	     | FxWhiteFlash
	     | FxGeneratingMap
	     | FxAssemblingMap
	     | FxPopulatingMap
	     | FxDurationSpell
	     | FxDeltazSpellStart
	     | FxTurnUndead
	     | FxArrowTrap
	     | FxVampirism
	     | FxManaBombCancel
	     | UpdateStream
	     | NewAlias
	     | AudioEvent
	     | AudioPlayerEvent
	     | TextMessage
	     | Inform
	     | Important
	     | ImportantAck                   { timestamp :: Timestamp }
	     | Mouse
	     | IncomingClient
	     | OutgoingClient
	     | GameSettings
	     | GameSettings2
	     | UpdateGuiGameSettings
	     | DoorAngle
	     | ObeliskCharge
	     | PentagramActivate
	     | ClientPredictLinear
	     | RequestMap
	     | CancelMap
	     | MapSendStart
	     | MapSendPacket
	     | MapSendAbort
	     | ServerCmd
	     | SysopPw
	     | SysopResult
	     | KeepAlive
	     | ReceivedMap
	     | ClientReady
	     | RequestSavePlayer
	     | XferMsg
	     | PlayerObj
	     | TeamMsg
	     | KickNotification
	     | TimeoutNotification
	     | ServerQuit
	     | ServerQuitAck
	     | Trade
	     | ChatKill
	     | MessagesKill
	     | SeqImportant
	     | ReportAbilityAward
	     | ReportAbilityState             { ability     :: Word8
                                              , isReady     :: Bool } --Word8
	     | ReportActiveAbilities
	     | Dialog
	     | ReportGuideAward
	     | InterestingId
	     | TimerStatus
	     | RequestTimerStatus
	     | JournalMsg
	     | ChapterEnd
	     | ReportAllLatency
	     | ReportFlagStatus
	     | ReportBallStatus
	     | ReportObjectPoison
	     | ReportMonitorCreature
	     | ReportUnmonitorCreature
	     | ReportTotalHealth              { extent :: Extent
                                              , hp     :: Word16
                                              , maxHp  :: Word16 }
	     | ReportTotalMana                { extent :: Extent
                                              , mp     :: Word16
                                              , maxMp  :: Word16 }
	     | ReportSpellStat
	     | ReportSecondaryWeapon
	     | ReportLastQuiver
	     | InfoBookData
	     | Social
	     | FadeBegin                      { unk1 :: Word8   -- always 1?
                                              , unk2 :: Word8 } -- always 1?
	     | MusicEvent
	     | MusicPushEvent
	     | MusicPopEvent
	     | PlayerDied
	     | PlayerRespawn
	     | ForgetDrawables
	     | ResetAbilities       { ability     :: Word8 }
	     | RateChange
	     | ReportCreatureCmd
	     | Vote
	     | StatMultipliers
	     | Gauntlet
	     | InventoryFail
             deriving (Eq, Show)

instance Enum Message where
    -- TODO template-haskell expression to define all pairingn of Messages to Events (enum on wire)
    fromEnum Event00{} = fromEnum E.Event00
    fromEnum Event01{} = fromEnum E.Event01
    fromEnum Event02{} = fromEnum E.Event02
    fromEnum Event03{} = fromEnum E.Event03
    fromEnum Event04{} = fromEnum E.Event04
    fromEnum Event05{} = fromEnum E.Event05
    fromEnum Event06{} = fromEnum E.Event06
    fromEnum Event07{} = fromEnum E.Event07
    fromEnum Event08{} = fromEnum E.Event08
    fromEnum Event09{} = fromEnum E.Event09
    fromEnum PlayerDisconnect{} = fromEnum E.PlayerDisconnect
    fromEnum Event0b{} = fromEnum E.Event0b
    fromEnum PingServer{} = fromEnum E.PingServer
    fromEnum PongClient{} = fromEnum E.PongClient
    fromEnum PlayerConnect{} = fromEnum E.PlayerConnect
    fromEnum Event0f{} = fromEnum E.Event0f
    fromEnum Event10{} = fromEnum E.Event10
    fromEnum Event11{} = fromEnum E.Event11
    fromEnum Event12{} = fromEnum E.Event12
    fromEnum Event13{} = fromEnum E.Event13
    fromEnum Event14{} = fromEnum E.Event14
    fromEnum Event15{} = fromEnum E.Event15
    fromEnum Event16{} = fromEnum E.Event16
    fromEnum Event17{} = fromEnum E.Event17
    fromEnum Event18{} = fromEnum E.Event18
    fromEnum Event19{} = fromEnum E.Event19
    fromEnum Event1a{} = fromEnum E.Event1a
    fromEnum Event1b{} = fromEnum E.Event1b
    fromEnum Event1c{} = fromEnum E.Event1c
    fromEnum Event1d{} = fromEnum E.Event1d
    fromEnum Event1e{} = fromEnum E.Event1e
    fromEnum PlayerJoin{} = fromEnum E.PlayerJoin
    fromEnum Event20{} = fromEnum E.Event20
    fromEnum Event21{} = fromEnum E.Event21
    fromEnum Event22{} = fromEnum E.Event22
    fromEnum Event23{} = fromEnum E.Event23
    fromEnum Event24{} = fromEnum E.Event24
    fromEnum Event25{} = fromEnum E.Event25
    fromEnum Event26{} = fromEnum E.Event26
    fromEnum PartialTimestamp{} = fromEnum E.PartialTimestamp
    fromEnum FullTimestamp{} = fromEnum E.FullTimestamp
    fromEnum NeedTimestamp{} = fromEnum E.NeedTimestamp
    fromEnum SimulatedTimestamp{} = fromEnum E.SimulatedTimestamp
    fromEnum UseMap{} = fromEnum E.UseMap
    fromEnum JoinData{} = fromEnum E.JoinData
    fromEnum NewPlayer{} = fromEnum E.NewPlayer
    fromEnum PlayerQuit{} = fromEnum E.PlayerQuit
    fromEnum SimpleObj{} = fromEnum E.SimpleObj
    fromEnum ComplexObj{} = fromEnum E.ComplexObj
    fromEnum DestroyObject{} = fromEnum E.DestroyObject
    fromEnum ObjectOutOfSight{} = fromEnum E.ObjectOutOfSight
    fromEnum ObjectInShadows{} = fromEnum E.ObjectInShadows
    fromEnum ObjectFriendAdd{} = fromEnum E.ObjectFriendAdd
    fromEnum ObjectFriendRemove{} = fromEnum E.ObjectFriendRemove
    fromEnum ResetFriends{} = fromEnum E.ResetFriends
    fromEnum EnableObject{} = fromEnum E.EnableObject
    fromEnum DisableObject{} = fromEnum E.DisableObject
    fromEnum DrawFrame{} = fromEnum E.DrawFrame
    fromEnum DestroyWall{} = fromEnum E.DestroyWall
    fromEnum OpenWall{} = fromEnum E.OpenWall
    fromEnum CloseWall{} = fromEnum E.CloseWall
    fromEnum ChangeOrAddWallMagic{} = fromEnum E.ChangeOrAddWallMagic
    fromEnum RemoveWallMagic{} = fromEnum E.RemoveWallMagic
    fromEnum PlayerInput{} = fromEnum E.PlayerInput
    fromEnum PlayerSetWaypoint{} = fromEnum E.PlayerSetWaypoint
    fromEnum ReportHealth{} = fromEnum E.ReportHealth
    fromEnum ReportHealthDelta{} = fromEnum E.ReportHealthDelta
    fromEnum ReportPlayerHealth{} = fromEnum E.ReportPlayerHealth
    fromEnum ReportItemHealth{} = fromEnum E.ReportItemHealth
    fromEnum ReportMana{} = fromEnum E.ReportMana
    fromEnum ReportPoison{} = fromEnum E.ReportPoison
    fromEnum ReportStamina{} = fromEnum E.ReportStamina
    fromEnum ReportStats{} = fromEnum E.ReportStats
    fromEnum ReportArmorValue{} = fromEnum E.ReportArmorValue
    fromEnum ReportGold{} = fromEnum E.ReportGold
    fromEnum ReportPickup{} = fromEnum E.ReportPickup
    fromEnum ReportModifiablePickup{} = fromEnum E.ReportModifiablePickup
    fromEnum ReportDrop{} = fromEnum E.ReportDrop
    fromEnum ReportLesson{} = fromEnum E.ReportLesson
    fromEnum ReportMundaneArmorEquip{} = fromEnum E.ReportMundaneArmorEquip
    fromEnum ReportMundaneWeaponEquip{} = fromEnum E.ReportMundaneWeaponEquip
    fromEnum ReportModifiableWeaponEquip{} = fromEnum E.ReportModifiableWeaponEquip
    fromEnum ReportModifiableArmorEquip{} = fromEnum E.ReportModifiableArmorEquip
    fromEnum ReportArmorDequip{} = fromEnum E.ReportArmorDequip
    fromEnum ReportWeaponDequip{} = fromEnum E.ReportWeaponDequip
    fromEnum ReportTreasureCount{} = fromEnum E.ReportTreasureCount
    fromEnum ReportFlagBallWinner{} = fromEnum E.ReportFlagBallWinner
    fromEnum ReportFlagWinner{} = fromEnum E.ReportFlagWinner
    fromEnum ReportDeathmatchWinner{} = fromEnum E.ReportDeathmatchWinner
    fromEnum ReportDeathmatchTeamWinner{} = fromEnum E.ReportDeathmatchTeamWinner
    fromEnum ReportEnchantment{} = fromEnum E.ReportEnchantment
    fromEnum ReportItemEnchantment{} = fromEnum E.ReportItemEnchantment
    fromEnum ReportLightColor{} = fromEnum E.ReportLightColor
    fromEnum ReportLightIntensity{} = fromEnum E.ReportLightIntensity
    fromEnum ReportZPlus{} = fromEnum E.ReportZPlus
    fromEnum ReportZMinus{} = fromEnum E.ReportZMinus
    fromEnum ReportEquip{} = fromEnum E.ReportEquip
    fromEnum ReportDequip{} = fromEnum E.ReportDequip
    fromEnum ReportAcquireSpell{} = fromEnum E.ReportAcquireSpell
    fromEnum ReportTarget{} = fromEnum E.ReportTarget
    fromEnum ReportCharges{} = fromEnum E.ReportCharges
    fromEnum ReportXStatus{} = fromEnum E.ReportXStatus
    fromEnum ReportPlayerStatus{} = fromEnum E.ReportPlayerStatus
    fromEnum ReportModifier{} = fromEnum E.ReportModifier
    fromEnum ReportStatModifier{} = fromEnum E.ReportStatModifier
    fromEnum ReportNpc{} = fromEnum E.ReportNpc
    fromEnum ReportClientStatus{} = fromEnum E.ReportClientStatus
    fromEnum ReportAnimationFrame{} = fromEnum E.ReportAnimationFrame
    fromEnum ReportAcquireCreature{} = fromEnum E.ReportAcquireCreature
    fromEnum ReportLoseCreature{} = fromEnum E.ReportLoseCreature
    fromEnum ReportExperience{} = fromEnum E.ReportExperience
    fromEnum ReportSpellAward{} = fromEnum E.ReportSpellAward
    fromEnum ReportSpellStart{} = fromEnum E.ReportSpellStart
    fromEnum ReportInventoryLoaded{} = fromEnum E.ReportInventoryLoaded
    fromEnum TryDrop{} = fromEnum E.TryDrop
    fromEnum TryGet{} = fromEnum E.TryGet
    fromEnum TryUse{} = fromEnum E.TryUse
    fromEnum TryEquip{} = fromEnum E.TryEquip
    fromEnum TryDequip{} = fromEnum E.TryDequip
    fromEnum TryTarget{} = fromEnum E.TryTarget
    fromEnum TryCreatureEvent{} = fromEnum E.TryCreatureEvent
    fromEnum TrySpell{} = fromEnum E.TrySpell
    fromEnum TryAbility{} = fromEnum E.TryAbility
    fromEnum TryCollide{} = fromEnum E.TryCollide
    fromEnum FxParticlefx{} = fromEnum E.FxParticlefx
    fromEnum FxPlasma{} = fromEnum E.FxPlasma
    fromEnum FxSummon{} = fromEnum E.FxSummon
    fromEnum FxSummonCancel{} = fromEnum E.FxSummonCancel
    fromEnum FxShield{} = fromEnum E.FxShield
    fromEnum FxBlueSparks{} = fromEnum E.FxBlueSparks
    fromEnum FxYellowSparks{} = fromEnum E.FxYellowSparks
    fromEnum FxCyanSparks{} = fromEnum E.FxCyanSparks
    fromEnum FxVioletSparks{} = fromEnum E.FxVioletSparks
    fromEnum FxExplosion{} = fromEnum E.FxExplosion
    fromEnum FxLesserExplosion{} = fromEnum E.FxLesserExplosion
    fromEnum FxCounterspellExplosion{} = fromEnum E.FxCounterspellExplosion
    fromEnum FxThinExplosion{} = fromEnum E.FxThinExplosion
    fromEnum FxTeleport{} = fromEnum E.FxTeleport
    fromEnum FxSmokeBlast{} = fromEnum E.FxSmokeBlast
    fromEnum FxDamagePoof{} = fromEnum E.FxDamagePoof
    fromEnum FxLightning{} = fromEnum E.FxLightning
    fromEnum FxEnergyBolt{} = fromEnum E.FxEnergyBolt
    fromEnum FxChainLightningBolt{} = fromEnum E.FxChainLightningBolt
    fromEnum FxDrainMana{} = fromEnum E.FxDrainMana
    fromEnum FxCharm{} = fromEnum E.FxCharm
    fromEnum FxGreaterHeal{} = fromEnum E.FxGreaterHeal
    fromEnum FxMagic{} = fromEnum E.FxMagic
    fromEnum FxSparkExplosion{} = fromEnum E.FxSparkExplosion
    fromEnum FxDeathRay{} = fromEnum E.FxDeathRay
    fromEnum FxSentryRay{} = fromEnum E.FxSentryRay
    fromEnum FxRicochet{} = fromEnum E.FxRicochet
    fromEnum FxJiggle{} = fromEnum E.FxJiggle
    fromEnum FxGreenBolt{} = fromEnum E.FxGreenBolt
    fromEnum FxGreenExplosion{} = fromEnum E.FxGreenExplosion
    fromEnum FxWhiteFlash{} = fromEnum E.FxWhiteFlash
    fromEnum FxGeneratingMap{} = fromEnum E.FxGeneratingMap
    fromEnum FxAssemblingMap{} = fromEnum E.FxAssemblingMap
    fromEnum FxPopulatingMap{} = fromEnum E.FxPopulatingMap
    fromEnum FxDurationSpell{} = fromEnum E.FxDurationSpell
    fromEnum FxDeltazSpellStart{} = fromEnum E.FxDeltazSpellStart
    fromEnum FxTurnUndead{} = fromEnum E.FxTurnUndead
    fromEnum FxArrowTrap{} = fromEnum E.FxArrowTrap
    fromEnum FxVampirism{} = fromEnum E.FxVampirism
    fromEnum FxManaBombCancel{} = fromEnum E.FxManaBombCancel
    fromEnum UpdateStream{} = fromEnum E.UpdateStream
    fromEnum NewAlias{} = fromEnum E.NewAlias
    fromEnum AudioEvent{} = fromEnum E.AudioEvent
    fromEnum AudioPlayerEvent{} = fromEnum E.AudioPlayerEvent
    fromEnum TextMessage{} = fromEnum E.TextMessage
    fromEnum Inform{} = fromEnum E.Inform
    fromEnum Important{} = fromEnum E.Important
    fromEnum ImportantAck{} = fromEnum E.ImportantAck
    fromEnum Mouse{} = fromEnum E.Mouse
    fromEnum IncomingClient{} = fromEnum E.IncomingClient
    fromEnum OutgoingClient{} = fromEnum E.OutgoingClient
    fromEnum GameSettings{} = fromEnum E.GameSettings
    fromEnum GameSettings2{} = fromEnum E.GameSettings2
    fromEnum UpdateGuiGameSettings{} = fromEnum E.UpdateGuiGameSettings
    fromEnum DoorAngle{} = fromEnum E.DoorAngle
    fromEnum ObeliskCharge{} = fromEnum E.ObeliskCharge
    fromEnum PentagramActivate{} = fromEnum E.PentagramActivate
    fromEnum ClientPredictLinear{} = fromEnum E.ClientPredictLinear
    fromEnum RequestMap{} = fromEnum E.RequestMap
    fromEnum CancelMap{} = fromEnum E.CancelMap
    fromEnum MapSendStart{} = fromEnum E.MapSendStart
    fromEnum MapSendPacket{} = fromEnum E.MapSendPacket
    fromEnum MapSendAbort{} = fromEnum E.MapSendAbort
    fromEnum ServerCmd{} = fromEnum E.ServerCmd
    fromEnum SysopPw{} = fromEnum E.SysopPw
    fromEnum SysopResult{} = fromEnum E.SysopResult
    fromEnum KeepAlive{} = fromEnum E.KeepAlive
    fromEnum ReceivedMap{} = fromEnum E.ReceivedMap
    fromEnum ClientReady{} = fromEnum E.ClientReady
    fromEnum RequestSavePlayer{} = fromEnum E.RequestSavePlayer
    fromEnum XferMsg{} = fromEnum E.XferMsg
    fromEnum PlayerObj{} = fromEnum E.PlayerObj
    fromEnum TeamMsg{} = fromEnum E.TeamMsg
    fromEnum KickNotification{} = fromEnum E.KickNotification
    fromEnum TimeoutNotification{} = fromEnum E.TimeoutNotification
    fromEnum ServerQuit{} = fromEnum E.ServerQuit
    fromEnum ServerQuitAck{} = fromEnum E.ServerQuitAck
    fromEnum Trade{} = fromEnum E.Trade
    fromEnum ChatKill{} = fromEnum E.ChatKill
    fromEnum MessagesKill{} = fromEnum E.MessagesKill
    fromEnum SeqImportant{} = fromEnum E.SeqImportant
    fromEnum ReportAbilityAward{} = fromEnum E.ReportAbilityAward
    fromEnum ReportAbilityState{} = fromEnum E.ReportAbilityState
    fromEnum ReportActiveAbilities{} = fromEnum E.ReportActiveAbilities
    fromEnum Dialog{} = fromEnum E.Dialog
    fromEnum ReportGuideAward{} = fromEnum E.ReportGuideAward
    fromEnum InterestingId{} = fromEnum E.InterestingId
    fromEnum TimerStatus{} = fromEnum E.TimerStatus
    fromEnum RequestTimerStatus{} = fromEnum E.RequestTimerStatus
    fromEnum JournalMsg{} = fromEnum E.JournalMsg
    fromEnum ChapterEnd{} = fromEnum E.ChapterEnd
    fromEnum ReportAllLatency{} = fromEnum E.ReportAllLatency
    fromEnum ReportFlagStatus{} = fromEnum E.ReportFlagStatus
    fromEnum ReportBallStatus{} = fromEnum E.ReportBallStatus
    fromEnum ReportObjectPoison{} = fromEnum E.ReportObjectPoison
    fromEnum ReportMonitorCreature{} = fromEnum E.ReportMonitorCreature
    fromEnum ReportUnmonitorCreature{} = fromEnum E.ReportUnmonitorCreature
    fromEnum ReportTotalHealth{} = fromEnum E.ReportTotalHealth
    fromEnum ReportTotalMana{} = fromEnum E.ReportTotalMana
    fromEnum ReportSpellStat{} = fromEnum E.ReportSpellStat
    fromEnum ReportSecondaryWeapon{} = fromEnum E.ReportSecondaryWeapon
    fromEnum ReportLastQuiver{} = fromEnum E.ReportLastQuiver
    fromEnum InfoBookData{} = fromEnum E.InfoBookData
    fromEnum Social{} = fromEnum E.Social
    fromEnum FadeBegin{} = fromEnum E.FadeBegin
    fromEnum MusicEvent{} = fromEnum E.MusicEvent
    fromEnum MusicPushEvent{} = fromEnum E.MusicPushEvent
    fromEnum MusicPopEvent{} = fromEnum E.MusicPopEvent
    fromEnum PlayerDied{} = fromEnum E.PlayerDied
    fromEnum PlayerRespawn{} = fromEnum E.PlayerRespawn
    fromEnum ForgetDrawables{} = fromEnum E.ForgetDrawables
    fromEnum ResetAbilities{} = fromEnum E.ResetAbilities
    fromEnum RateChange{} = fromEnum E.RateChange
    fromEnum ReportCreatureCmd{} = fromEnum E.ReportCreatureCmd
    fromEnum Vote{} = fromEnum E.Vote
    fromEnum StatMultipliers{} = fromEnum E.StatMultipliers
    fromEnum Gauntlet{} = fromEnum E.Gauntlet
    fromEnum InventoryFail{} = fromEnum E.InventoryFail
    -- There can be no toEnum. The parser will have to look into the contents
    -- of a bytestring to find the eventId and construct the Message
    -- appropriately. A Message can't be sensibly constructed from an Int id alone.
    toEnum = undefined

putEvent = putWord8 . fromIntegral . fromEnum
getEvent = getWord8 >>= return . toEnum . fromIntegral

instance Serialize Message where
    put m = do
        -- TODO: handle rest of header in up/downstream conduits? (player Word8, unknown Word8)
        putWord8 0 -- FIXME playerid
        putWord8 0 -- unknown
        putEvent m
        case m of
            PongClient{..} -> do
                putWord8 actPlayers
                putWord8 maxPlayers
                putWord8    unkB1
                putWord8    unkB2
                putWord8    unkB3
                putWord8    unkB4
                putWord8    unkB5
                putByteString mapName
                putByteString unkBS1
                putWord32le timestamp
                putAllowedSpells okSpells
                putByteString unkBS2
                putByteString gameName
            ReportHealth{..} -> do
                putExtent extent
                putWord16le health
            ReportMana{..} -> do
                putExtent extent
                putWord16le mana
            ReportClientStatus{..} -> do
                putExtent extent
                putWord32le . fromIntegral . fromEnum $ isObserving
            ReportAbilityState{..} -> do
                putWord8 ability
                putWord8 . fromIntegral . fromEnum $ isReady
            ResetAbilities{..} -> do
                putWord8 ability
    get = do
        player <- getWord8
        unk <- getWord8
        event <- getEvent
        case event of
            E.PingServer -> do
                unkB1 <- getWord8
                unkB2 <- getWord8
                unkB3 <- getWord8
                unkB4 <- getWord8
                unkB5 <- getWord8
                timestamp <- getTimestamp
                return PingServer{..}
            E.ReportHealth -> do
                extent <- getExtent
                health <- getWord16le
                return ReportHealth{..}
            E.ReportMana -> do
                extent <- getExtent
                mana <- getWord16le
                return ReportMana{..}
            E.ReportClientStatus -> do
                extent <- getExtent
                isObserving <- getWord32le >>= return . toEnum . fromIntegral
                return ReportClientStatus{..}
            E.ReportAbilityState -> do
                ability <- getWord8
                isReady <- getWord8 >>= return . toEnum . fromIntegral
                return ReportAbilityState{..}
            E.ResetAbilities -> do
                ability <- getWord8
                return ResetAbilities{..}
            unknown -> do
                error ("unknown event id=" ++ show unknown)
