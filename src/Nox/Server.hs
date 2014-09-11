{-# LANGUAGE RecordWildCards #-}
module Nox.Server (
    noxd
) where

import Network.Socket
import Data.Conduit
import Data.Conduit.Network.UDP
import Debug.Trace
import Data.Word
import Data.ByteString

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
    mbData <- await
    case mbData of
        Just msg@Message{..} -> trace (show msgData) $ yield msg
        Nothing -> return ()

