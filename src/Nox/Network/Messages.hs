{-# LANGUAGE RecordWildCards #-}
module Nox.Network.Messages where

import Data.Word
import Data.ByteString
import Data.Serialize

import Nox.Network.WireProtocol hiding (Event(..))
import qualified Nox.Network.WireProtocol as E (Event(..))
import Nox.Game.Player

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
                                       , unkBS1 :: ByteString --  26 of gibberish
                                       , timeStamp :: Word32
                                       , unkBS2 :: ByteString --  24 of gibberish
                                       , testStr :: ByteString } -- 5 including null term
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
    fromEnum NewPlayer{}          = fromEnum E.NewPlayer
    fromEnum ReportHealth{}       = fromEnum E.ReportHealth
    fromEnum ReportMana{}         = fromEnum E.ReportMana
    fromEnum ReportClientStatus{} = fromEnum E.ReportClientStatus
    fromEnum ReportAbilityState{} = fromEnum E.ReportAbilityState
    fromEnum ResetAbilities{}     = fromEnum E.ResetAbilities
    -- There can be no toEnum. The parser will have to look into the contents
    -- of a bytestring to find the eventId and construct the Message
    -- appropriately. A Message can't be sensibly constructed from an Int id alone.
    toEnum = undefined

putEvent = putWord8 . fromIntegral . fromEnum
getEvent = getWord8 >>= return . toEnum . fromIntegral

instance Serialize Message where
    put m = do
        -- TODO: handle rest of header in up/downstream conduits? (player Word8, unknown Word8)
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
                put         mapName-- 9 long (null term?)
                put         unkBS1--  26 of gibberish
                putWord32le timeStamp
                put         unkBS2--  24 of gibberish
                put         testStr-- 5 including null term
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
            E.ReportHealth{..} -> do
                extent <- getExtent
                health <- getWord16le
                return ReportHealth{..}
            E.ReportMana{..} -> do
                extent <- getExtent
                mana <- getWord16le
                return ReportMana{..}
            E.ReportClientStatus -> do
                extent <- getExtent
                isObserving <- getWord32le >>= return . toEnum . fromIntegral
                return ReportClientStatus{..}
            E.ReportAbilityState{..} -> do
                ability <- getWord8
                isReady <- getWord8 >>= return . toEnum . fromIntegral
                return ReportAbilityState{..}
            E.ResetAbilities{..} -> do
                ability <- getWord8
                return ResetAbilities{..}
