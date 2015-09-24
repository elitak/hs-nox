{-# LANGUAGE RecordWildCards #-}
module Nox.Network.Server (
    noxd
) where

import Network.Socket
import Data.Conduit
import Data.Conduit.Network.UDP
import Debug.Trace
import Data.Word
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Serialize
import Data.Flags
import Data.Yaml
import Control.Concurrent hiding (yield)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid

import Nox.Network.WireProtocol
import Nox.Network.WireProtocol.ServerInfo

gamePort = 18590
maxPayload = 1500

data Config = Config { serverStatus :: ServerStatus
                     }

data ServerStatus = ServerStatus { bannedSpells :: [AllowedSpells]
                                 }

instance Monoid AllowedSpells where
    mempty = noFlags
    mappend = (.+.)

refreshConfig confVar = do
    let newConf = Config { serverStatus = ServerStatus { bannedSpells = [lightning] } }
    putMVar confVar newConf

noxd :: IO ()
noxd = withSocketsDo $ do
    confVar <- newEmptyMVar
    refreshConfig confVar
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet gamePort iNADDR_ANY)
    -- TODO Insert xor code tinto this pipeline somewhere for transparent ciphering.
    --      It has to be used conditionally, based on what msgtype arrives.
    --let loop sock = (sourceSocket sock maxPayload $= decipher =$= handleMsg =$= encipher $$ sinkToSocket sock) >> loop sock
    -- TODO Am i even getting anything out of using conduits here?
    let loop s = ( sourceSocket s maxPayload
                $= handleMsg confVar
                $$ sinkToSocket s
                 ) >> loop s
    loop sock

--data NoxMessage = NoxMessage { sender :: !SockAddr, xor :: !Word8, payload :: !ByteString }
--decipher = do
--    mbMsg <- await
--    case mbMsg of
--        Just msg@Message{..} -> trace (show msgData) $ yield msg
--        Nothing -> return ()

handleMsg confVar = do
    -- XXX really re-read config every loop??
    conf <- liftIO $ readMVar confVar
    mbMsg <- await
    case mbMsg of
        Just Message{..} -> do
        -- TODO Yampa needs to replace this to handle events
            case runGet get msgData of
                Right PingServer{..} -> do
                    let resp = PongClient { actPlayers = 0
                                          , maxPlayers = 1
                                          -- length must be 9, inc null; otherwise text prints until null encountered
                                          , mapName = BC.pack "FakeMap"
                                          , gameName = BC.pack "noxd" -- maxlen is 31 inc null
                                          , timestamp = timestamp
                                          , unkB1 = 0x0f --notmask
                                          , unkB2 = 0x0f --notmask
                                          , okWeapons = allFlags
                                          , unkBS1 = pack [ 0x00 -- 0=640res,1=800,2=1024, changes to high6? bits here crashes game
                                                          , 0x00
	                                                  , 0x00
	                                                  , 0x55
	                                                  , 0x00
	                                                  , 0x9A
	                                                  , 0x03
	                                                  , 0x01
	                                                  , 0x00
	                                                  , 0x07
	                                                  , 0x21
                                                          , 0x03
	                                                  , 0x10]
                                          , okArmors = allFlags
                                          , unkBS3 = pack [
	                                                    0xFF --notmask
                                                          , 0xFF --notmask
                                                          , 0xFF --notmask!
                                                          , 0xFF --notmask
	                                                  , 0xC0 --notmask
                                                          , 0x00
	                                                  , 0xD4
                                                          , 0x00]
                                          , okSpells = allBut (mconcat (bannedSpells . serverStatus $ conf))
                                          , unkBS2 = pack [
                                                            0xFF --notmask?
                                                          , 0xFF --notmask
                                                          , 0x84
                                                          , 0x82
                                                          , 0xD3
                                                          , 0x01 ]
                                          }
                    let msgData = runPut $ put resp
                    trace ("resp msg is " ++ show resp) $ 
                        trace ("resp len is " ++ show (Prelude.length . unpack $ msgData)) $ 
                        trace ("resp data is " ++ (show . unpack $ msgData)) $ 
                        yield Message{..}
                Right other ->
                    trace "unhandled message"
                    trace (show other) $
                    trace ("msg data was " ++ (show . unpack $ msgData)) $ 
                    return ()
                Left err ->
                    trace err $
                    trace ("msg data was " ++ (show . unpack $ msgData)) $ 
                    return ()
        Nothing -> return ()
