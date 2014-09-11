{-# LANGUAGE RecordWildCards #-}
module Main where

--import Nox.Network.WireProtocol
--import Nox.Crypt
--import Nox.Zip

import Data.Conduit
--import Data.Conduit.Binary
--import Control.Monad.Trans.Resource (runResourceT)

import Network.Socket
import Data.Conduit.Network.UDP
import System.Posix.Process
import System.Posix.IO
import System.Exit
import Debug.Trace

gamePort = 18590
maxPayload = 1500

-- TODO Consider using Data.Conduit.Network for handling connections and doing the xor-transformation to the stream.
--      + conduit-cereal??
main = do
    putStrLn "noxd v0.1" -- v1.0 will be all original nox msgs supported, if not game modes (like Quest)

    --runResourceT $ sourceFile "gamedata.bin" $= decrypt GameData $$ sinkFile "gamedata.txt"
    --runResourceT $ sourceFile "So_Beach.map" $= decrypt Map $$ sinkFile "map.out"
    --runResourceT $ sourceFile "in.map" $= compress $$ sinkFile "out.nxz"

    -- use this to background the process
    --pid <- forkProcess child
    --exitImmediately ExitSuccess
    noxd

child = do
           --changeWorkingDirectory "/"
           --setFileCreationMask 0
           mapM_ closeFd [stdInput, stdOutput, stdError]
           nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
           mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
           closeFd nullFd
           createSession     -- This child becomes a process and session
                             -- group leader. This prevents the child of
                             -- this process (the daemon) from
                             -- ever getting a controlling terminal.
           pid' <- forkProcess noxd
           exitImmediately ExitSuccess

noxd = withSocketsDo $ do
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet gamePort iNADDR_ANY)
    let loop sock = do
            sourceSocket sock maxPayload $= handleMsg $$ sinkToSocket sock
            loop sock
    loop sock

handleMsg = do
    mbMsg <- await
    case mbMsg of
        Just msg@Message{..} -> trace (show msgData) $ yield msg
        Nothing -> return ()

-- TODO Test performance early and often, so that I can make sure the server binary runs as-fast or faster than original in terms of netcode/responsiveness
-- In particular, I worry that an elegant, Alternative-based (de)serializer might be slow (or whatever solution I pick).
