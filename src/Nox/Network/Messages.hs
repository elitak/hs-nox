{-# LANGUAGE RecordWildCards #-}
module Nox.Network.Messages where

import Data.Word
import Data.ByteString
import Nox.Network.WireProtocol hiding (Event(..))
import qualified Nox.Network.WireProtocol as E (Event(..))
import Data.Serialize

-- REMEMBER to keep these in enum order else resorting them gets difficult...
data Message = NewPlayer            { extent      :: Extent
                                    , info        :: ByteString }
	     | ReportHealth         { extent      :: Extent
                                    , health      :: Word16 }
	     | ReportMana           { extent      :: Extent
                                    , mana        :: Word16 }
	     | ReportClientStatus   { extent      :: Extent
                                    , isObserving :: Bool } --Word32
	     | ReportAbilityState   { ability     :: Word8
                                    , isReady     :: Bool } --Word8
	     | ResetAbilities       { ability     :: Word8 }
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
