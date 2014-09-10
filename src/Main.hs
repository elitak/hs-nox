module Main where

import Nox.Network.WireProtocol
import Nox.Crypt
import Nox.Zip

import Data.Conduit
import Data.Conduit.Binary
import Control.Monad.Trans.Resource (runResourceT)

-- TODO Consider using Data.Conduit.Network for handling connections and doing the xor-transformation to the stream.
--      + conduit-cereal??
main = do
    putStrLn "noxd v0.1" -- v1.0 will be all original nox msgs supported, if not game modes (like Quest)
    --runResourceT $ sourceFile "gamedata.bin" $= decrypt GameData $$ sinkFile "gamedata.txt"
    --runResourceT $ sourceFile "So_Beach.map" $= decrypt Map $$ sinkFile "map.out"
    runResourceT $ sourceFile "So_Beach.map" $= compress $$ sinkFile "map.out"


-- TODO Test performance early and often, so that I can make sure the server binary runs as-fast or faster than original in terms of netcode/responsiveness
-- In particular, I worry that an elegant, Alternative-based (de)serializer might be slow (or whatever solution I pick).
