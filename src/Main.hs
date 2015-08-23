module Main where

--import Data.Conduit
--import Data.Conduit.Binary
--import Control.Monad.Trans.Resource (runResourceT)

import System.Exit
import Control.Concurrent
import Nox.Network.Server

main = do
    putStrLn "noxd v0.1" -- v1.0 will be all original nox msgs supported, if not game modes (like Quest)

    --runResourceT $ sourceFile "gamedata.bin" $= decrypt GameData $$ sinkFile "gamedata.txt"
    --runResourceT $ sourceFile "So_Beach.map" $= decrypt Map $$ sinkFile "map.out"
    --runResourceT $ sourceFile "in.map" $= compress $$ sinkFile "out.nxz"

    noxd

-- TODO Test performance early and often, so that I can make sure the server binary runs as-fast or faster than original in terms of netcode/responsiveness
-- In particular, I worry that an elegant, Alternative-based (de)serializer might be slow (or whatever solution I pick).

--children :: MVar [MVar ()]
--children = unsafePerformIO (newMVar [])
--
--waitForChildren :: IO ()
--waitForChildren = do
--  cs <- takeMVar children
--  case cs of
--    []   -> return ()
--    m:ms -> do
--       putMVar children ms
--       takeMVar m
--       waitForChildren
--
--forkChild :: IO () -> IO ThreadId
--forkChild io = do
--    mvar <- newEmptyMVar
--    childs <- takeMVar children
--    putMVar children (mvar:childs)
--    forkFinally io (\_ -> putMVar mvar ())
--
--main =
--    later waitForChildren $
--    putStrLn "noxd v0.1" -- v1.0 will be all original nox msgs supported, if not game modes (like Quest)
