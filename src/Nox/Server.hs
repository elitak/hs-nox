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
    -- insert xor code into this pipeline somewhere? it has to be used conditionally, based on what msgtype arrives
    --let loop sock = (sourceSocket sock maxPayload $= decipher =$= handleMsg =$= encipher $$ sinkToSocket sock) >> loop sock
    let loop sock = (sourceSocket sock maxPayload $= handleMsg $$ sinkToSocket sock) >> loop sock
    loop sock

--decipher = do
--    mbMsg <- await
--    case mbMsg of
--        Just msg@Message{..} -> trace (show msgData) $ yield msg
--        Nothing -> return ()

data NoxMessage = NoxMessage { sender :: !SockAddr, xor :: !Word8, payload :: !ByteString }

handleMsg = do
    mbData <- await
    case mbData of
        Just msg@Message{..} -> trace (show msgData) $ yield msg
        Nothing -> return ()

