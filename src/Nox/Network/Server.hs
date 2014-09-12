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

import Nox.Network.Messages
import qualified Nox.Network.Messages as NM

gamePort = 18590
maxPayload = 1500

noxd = withSocketsDo $ do
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet gamePort iNADDR_ANY)
    -- TODO Insert xor code tinto this pipeline somewhere for transparent ciphering.
    --      It has to be used conditionally, based on what msgtype arrives.
    --let loop sock = (sourceSocket sock maxPayload $= decipher =$= handleMsg =$= encipher $$ sinkToSocket sock) >> loop sock
    -- TODO Am i even getting anything out of using conduits here?
    let loop s = ( sourceSocket s maxPayload
                $= handleMsg
                $$ sinkToSocket s
                 ) >> loop s
    loop sock

--data NoxMessage = NoxMessage { sender :: !SockAddr, xor :: !Word8, payload :: !ByteString }
--decipher = do
--    mbMsg <- await
--    case mbMsg of
--        Just msg@Message{..} -> trace (show msgData) $ yield msg
--        Nothing -> return ()

handleMsg = do
    mbMsg <- await
    case mbMsg of
        Just Message{..} -> do
        -- TODO Yampa needs to replace this to handle events
            case (runGet get msgData) of
                Right PingServer{..} -> do
                    let resp = PongClient { actPlayers = 0
                                          , maxPlayers = 1
                                          , mapName = BC.pack "Estate\0\0\0"
                                          , gameName = BC.pack "Test\0"
                                          , timestamp = timestamp
                                          , unkB1 = 0x0f
                                          , unkB2 = 0x0f
                                          , unkB3 = 0xff
                                          , unkB4 = 0xff
                                          , unkB5 = 0xff
                                          -- Some of this is the banned spell,weapon,armor bitmask
                                          , unkBS1 = pack [ 0x01
                                                          , 0x00
	                                                  , 0x00
	                                                  , 0x55
	                                                  , 0x00
	                                                  , 0x9A
	                                                  , 0x03
	                                                  , 0x01
	                                                  , 0x00
	                                                  , 0x07
	                                                  , 0x21 , 0x03
	                                                  , 0x10
	                                                  , 0xFF -- armors
                                                          , 0xFF , 0xFF
                                                          , 0xFF  -- armors
	                                                  , 0xFF , 0xFF
                                                          , 0xFF --notmask
                                                          , 0xFF --notmask
	                                                  , 0xC0 --notmask
                                                          , 0x00
	                                                  , 0xD4 , 0x00]
                                          , okSpells = allBut (markLocation3 .+. lightning .+. earthquake)
                                          , unkBS2 = pack [ 0xFF , 0xFF , 0xFF , 0xFF
                                                          , 0xFF , 0xFF , 0xFF , 0xFF
                                                          , 0xFF -- Wall in here
                                                          , 0xFF -- 8=TeleportToMarker, rest do nothing
                                                          , 0xFF --notmask?
                                                          , 0xFF
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
