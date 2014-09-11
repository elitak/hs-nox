{-# LANGUAGE RecordWildCards #-}
module Nox.Network.Messages where

import Data.Word
import Data.ByteString
import Nox.Network.WireProtocol hiding (Event(..))
import qualified Nox.Network.WireProtocol as E (Event(..))
import Data.Serialize

data Message = NewPlayer            { extent      :: Extent
                                    , info        :: ByteString }
	     | ReportClientStatus   { extent      :: Extent
                                    , isObserving :: Bool } --Word32
	     | ResetAbilities       { ability     :: Word8 }
	     | ReportAbilityState   { ability     :: Word8
                                    , isReady     :: Bool } --Word8
	     | ReportHealth         { extent      :: Extent
                                    , health      :: Word16 }
	     | ReportMana           { extent      :: Extent
                                    , mana        :: Word16 }
             -- FIXME: struct wrong
             -- | PlayerConnect        { id          :: Word8 }
             deriving (Eq, Show)

instance Enum Message where
    -- TODO template-haskell expression to define all pairingn of Messages to Events (enum on wire)
    fromEnum NewPlayer{}     = fromEnum E.NewPlayer
    fromEnum ReportClientStatus{}     = fromEnum E.ReportClientStatus
    --fromEnum PlayerConnect{} = fromEnum E.PlayerConnect
    -- There can be no toEnum. The parser will have to look into the contents
    -- of a bytestring to find the eventId and construct the Message
    -- appropriately. A Message can't be sensibly constructed from an Int id alone.
    toEnum = undefined

putEvent = putWord8 . fromIntegral . fromEnum
getEvent = getWord8 >>= return . toEnum . fromIntegral

instance Serialize Message where
    put m = do
        putEvent m
        case m of
            ResetAbilities{..} -> do
                putWord8 ability
            ReportAbilityState{..} -> do
                putWord8 ability
                putWord8 . fromIntegral . fromEnum $ isReady
            ReportClientStatus{..} -> do
                putExtent extent
                putWord32le . fromIntegral . fromEnum $ isObserving
            ReportHealth{..} -> do
                putExtent extent
                putWord16le health
            ReportMana{..} -> do
                putExtent extent
                putWord16le mana
    get = do
        event <- getEvent
        case event of
            E.ResetAbilities{..} -> do
                ability <- getWord8
                return ResetAbilities{..}
            E.ReportAbilityState{..} -> do
                ability <- getWord8
                isReady <- getWord8 >>= return . toEnum . fromIntegral
                return ReportAbilityState{..}
            E.ReportClientStatus -> do
                extent <- getExtent
                isObserving <- getWord32le >>= return . toEnum . fromIntegral
                return ReportClientStatus{..}
            E.ReportHealth{..} -> do
                extent <- getExtent
                health <- getWord16le
                return ReportHealth{..}
            E.ReportMana{..} -> do
                extent <- getExtent
                mana <- getWord16le
                return ReportMana{..}
