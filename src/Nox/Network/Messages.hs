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
	     | AbilityState         { ability     :: Word8
                                    , isReady     :: Bool } --Word8
	     | ReportHealth         { extent      :: Extent
                                    , health      :: Word16 }
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
    put m@ResetAbilities{..} = do
        putWord8 ability
    put m@AbilityState{..} = do
        putWord8 ability
        putWord8 . fromIntegral . fromEnum $ isReady
    put m@ReportClientStatus{..} = do
        putEvent m
        putExtent extent
        putWord32le . fromIntegral . fromEnum $ isObserving
    put m@ReportHealth{..} = do
        putExtent extent
        putWord16le health
    get = getMessage

getMessage = do
    event <- getEvent
    case event of
        E.ReportClientStatus -> do
            extent <- getExtent
            isObserving <- getWord32le >>= return . toEnum . fromIntegral
            return ReportClientStatus{..}
