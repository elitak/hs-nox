module Main where

import Nox.Network.WireProtocol
import Nox.Crypt

import Data.Conduit
import Data.Conduit.Binary
import Control.Monad.Trans.Resource (runResourceT)

-- TODO Consider using Data.Conduit.Network for handling connections and doing the xor-transformation to the stream.
--      + conduit-cereal??
main = do
    putStrLn "noxd v0.1" -- v1.0 will be all original nox msgs supported, if not game modes (like Quest)
    let conduit = sourceFile "gamedata.bin" $= decrypt GameData $$ sinkFile "gamedata.out"
    runResourceT conduit
