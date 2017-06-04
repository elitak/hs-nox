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

import Nox.Network.WireProtocol
import Nox.Network.WireProtocol.ServerInfo

gamePort = 18590
maxPayload = 1500

noxd = withSocketsDo $ do
    sock <- socket AF_INET Datagram 0
    bind sock (SockAddrInet gamePort iNADDR_ANY)
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
            case runGet get msgData of
                Right PingServer{..} -> do
                    let resp = PongClient { actPlayers = 0
                                          , maxPlayers = 1
                                          -- VULN length must be 9, inc null; otherwise text prints until null encountered
                                          , mapName = BC.pack "FakeMap"
                                          , gameName = BC.pack "noxd" -- maxlen is 31 inc null
                                          , timestamp = timestamp
                                          -- TODO though most of the remaniing unknowns do not appear in server info, look at hosting options in the client (when launching from social map) to get ideas on what these remaining flags could indicate. stuff like "closed game" "min/max ping" etc
                                          -- I should dump and diff responses from the real listenserver using wireshark + a binary template tool, twiddling 1 bit at a time between dumps in the server options menu
                                          -- one of these "15" values is lesson limit, prly!
                                          , unkB1 = 0x0f --notmask
                                          , unkB2 = 0x0f --notmask

                                          -- TODO halbred with the heart of nox is always banned? tried flipping bits adjacent to either end with no success in finding the 2 missing halberd bits
                                          , okWeapons = allFlags
                                          , resolution = Res1024
                                          , unkBS1 = pack [ 0x00
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
                                          -- the next 0xffs could be part of the armor masks, just all unused?
                                          , unkBS3 = pack [ 0xFF --notmask
                                                          , 0xFF --notmask
                                                          , 0xFF --notmask!
                                                          , 0xFF --notmask
                                                          , 0xC0 --notmask
                                                          , 0x00
                                                          , 0xD4
                                                          , 0x00]
                                          -- need test case like this than ensures all unused are actually unused
                                          --, okSpells = allBut $ unuSp0 .+. unuSp2 .+. unuSp3 .+. unuSp6 .+. unuSp7.+. unuSp11 .+. unuSp15 .+. unuSp17 .+. unuSp18 .+. unuSp20 .+. unuSp53 .+. unuSp55 .+. unuSp57 .+. unuSp59 .+. unuSp45 .+. unuSp25 .+. unuSp68
                                          , okSpells = allFlags
                                          , unkBS2 = pack [ 0xFF --notmask?
                                                          , 0xFF --notmask
                                                          , 0x84
                                                          , 0x82
                                                          , 0xD3
                                                          , 0x01 ]
                                                           -- NB better way to RE multiple values at once would be to return multiple server infos to client with diff names for each so that i can cycle thru many attempts simultaneously!!!
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
