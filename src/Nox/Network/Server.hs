{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import GHC.Generics
import Control.Monad (mzero)
import Control.Applicative (pure)

import Nox.Network.WireProtocol
import Nox.Network.WireProtocol.ServerInfo

gamePort = 18590
maxPayload = 1500

data Config = Config { serverStatus :: ServerStatus
                     } deriving (Eq, Show, Generic)

data ServerStatus = ServerStatus { bannedSpells :: [AllowedSpells]
                                 } deriving (Eq, Show, Generic)

instance FromJSON Config
instance FromJSON ServerStatus

instance FromJSON AllowedSpells where
    -- TODO: TH for these aliases
    parseJSON (String "unkSp0")            = pure unkSp0
    parseJSON (String "anchor")            = pure anchor
    parseJSON (String "unkSp2")            = pure unkSp2
    parseJSON (String "unkSp3")            = pure unkSp3
    parseJSON (String "blink")             = pure blink
    parseJSON (String "burn")              = pure burn
    parseJSON (String "unkSp6")            = pure unkSp6
    parseJSON (String "unkSp7")            = pure unkSp7
    parseJSON (String "channelLife")      = pure channelLife
    parseJSON (String "charmCreature")     = pure charmCreature
    parseJSON (String "ringOfFire")        = pure ringOfFire
    parseJSON (String "unkSp11")           = pure unkSp11
    parseJSON (String "confuse")           = pure confuse
    parseJSON (String "counterspell")      = pure counterspell
    parseJSON (String "curePoison")        = pure curePoison
    parseJSON (String "unkSp15")           = pure unkSp15
    parseJSON (String "deathRay")          = pure deathRay
    parseJSON (String "unkSp17")           = pure unkSp17
    parseJSON (String "unkSp18")           = pure unkSp18
    parseJSON (String "detonateSeenTraps") = pure detonateSeenTraps
    parseJSON (String "unkSp20")           = pure unkSp20
    parseJSON (String "dispelUndead")      = pure dispelUndead
    parseJSON (String "drainMana")         = pure drainMana
    parseJSON (String "earthquake")        = pure earthquake
    parseJSON (String "energyBolt")        = pure energyBolt
    parseJSON (String "unkSp25")           = pure unkSp25
    parseJSON (String "fear")              = pure fear
    parseJSON (String "fireball")          = pure fireball
    parseJSON (String "unkSp28")           = pure unkSp28
    parseJSON (String "unkSp29")           = pure unkSp29
    parseJSON (String "unkSp30")           = pure unkSp30
    parseJSON (String "unkSp31")           = pure unkSp31
    parseJSON (String "unkSp32")           = pure unkSp32
    parseJSON (String "unkSp33")           = pure unkSp33
    parseJSON (String "unkSp34")           = pure unkSp34
    parseJSON (String "unkSp35")           = pure unkSp35
    parseJSON (String "unkSp36")           = pure unkSp36
    parseJSON (String "unkSp37")           = pure unkSp37
    parseJSON (String "unkSp38")           = pure unkSp38
    parseJSON (String "invisibility")      = pure invisibility
    parseJSON (String "invulnerability")   = pure invulnerability
    parseJSON (String "lesserHeal")        = pure lesserHeal
    parseJSON (String "light")             = pure light
    parseJSON (String "lightning")         = pure lightning
    parseJSON (String "lock")              = pure lock
    parseJSON (String "unkSp45")           = pure unkSp45
    parseJSON (String "markLocation1")     = pure markLocation1
    parseJSON (String "markLocation2")     = pure markLocation2
    parseJSON (String "markLocation3")     = pure markLocation3
    parseJSON (String "markLocation4")     = pure markLocation4
    parseJSON (String "unkSp50")           = pure unkSp50
    parseJSON (String "unkSp51")           = pure unkSp51
    parseJSON (String "unkSp52")           = pure unkSp52
    parseJSON (String "unkSp53")           = pure unkSp53
    parseJSON (String "unkSp54")           = pure unkSp54
    parseJSON (String "unkSp55")           = pure unkSp55
    parseJSON (String "unkSp56")           = pure unkSp56
    parseJSON (String "unkSp57")           = pure unkSp57
    parseJSON (String "unkSp58")           = pure unkSp58
    parseJSON (String "unkSp59")           = pure unkSp59
    parseJSON (String "unkSp60")           = pure unkSp60
    parseJSON (String "unkSp61")           = pure unkSp61
    parseJSON (String "unkSp62")           = pure unkSp62
    parseJSON (String "unkSp63")           = pure unkSp63
    parseJSON (String "unkSp64")           = pure unkSp64
    parseJSON (String "unkSp65")           = pure unkSp65
    parseJSON (String "unkSp66")           = pure unkSp66
    parseJSON (String "unkSp67")           = pure unkSp67
    parseJSON (String "unkSp68")           = pure unkSp68
    parseJSON (String "unkSp69")           = pure unkSp69
    parseJSON (String "unkSp70")           = pure unkSp70
    parseJSON (String "unkSp71")           = pure unkSp71
    parseJSON (String "unkSp72")           = pure unkSp72
    parseJSON (String "unkSp73")           = pure unkSp73
    parseJSON (String "unkSp74")           = pure unkSp74
    parseJSON (String "unkSp75")           = pure unkSp75
    parseJSON (String "unkSp76")           = pure unkSp76
    parseJSON (String "unkSp77")           = pure unkSp77
    parseJSON (String "unkSp78")           = pure unkSp78
    parseJSON (String "unkSp79")           = pure unkSp79
    parseJSON (String "unkSp80")           = pure unkSp80
    parseJSON (String "unkSp81")           = pure unkSp81
    parseJSON (String "unkSp82")           = pure unkSp82
    parseJSON (String "unkSp83")           = pure unkSp83
    parseJSON (String "unkSp84")           = pure unkSp84
    parseJSON (String "unkSp85")           = pure unkSp85
    parseJSON (String "unkSp86")           = pure unkSp86
    parseJSON (String "unkSp87")           = pure unkSp87
    parseJSON (String "unkSp88")           = pure unkSp88
    parseJSON (String "unkSp89")           = pure unkSp89
    parseJSON (String "unkSp90")           = pure unkSp90
    parseJSON (String "unkSp91")           = pure unkSp91
    parseJSON (String "unkSp92")           = pure unkSp92
    parseJSON (String "unkSp93")           = pure unkSp93
    parseJSON (String "unkSp94")           = pure unkSp94
    parseJSON (String "unkSp95")           = pure unkSp95
    parseJSON (String "unkSp96")           = pure unkSp96
    parseJSON (String "unkSp97")           = pure unkSp97
    parseJSON (String "unkSp98")           = pure unkSp98
    parseJSON (String "unkSp99")           = pure unkSp99
    parseJSON (String "unkSp100")          = pure unkSp100
    parseJSON (String "unkSp101")          = pure unkSp101
    parseJSON (String "unkSp102")          = pure unkSp102
    parseJSON (String "unkSp103")          = pure unkSp103
    parseJSON (String "unkSp104")          = pure unkSp104
    parseJSON (String "unkSp105")          = pure unkSp105
    parseJSON (String "unkSp106")          = pure unkSp106
    parseJSON (String "unkSp107")          = pure unkSp107
    parseJSON (String "unkSp108")          = pure unkSp108
    parseJSON (String "unkSp109")          = pure unkSp109
    parseJSON (String "unkSp110")          = pure unkSp110
    parseJSON (String "unkSp111")          = pure unkSp111
    parseJSON (String "unkSp112")          = pure unkSp112
    parseJSON (String "unkSp113")          = pure unkSp113
    parseJSON (String "unkSp114")          = pure unkSp114
    parseJSON (String "unkSp115")          = pure unkSp115
    parseJSON (String "unkSp116")          = pure unkSp116
    parseJSON (String "unkSp117")          = pure unkSp117
    parseJSON (String "unkSp118")          = pure unkSp118
    parseJSON (String "unkSp119")          = pure unkSp119
    parseJSON (String "unkSp120")          = pure unkSp120
    parseJSON (String "unkSp121")          = pure unkSp121
    parseJSON (String "teleportToMarker1") = pure teleportToMarker1
    parseJSON (String "teleportToMarker2") = pure teleportToMarker2
    parseJSON (String "teleportToMarker3") = pure teleportToMarker3
    parseJSON (String "teleportToMarker4") = pure teleportToMarker4
    parseJSON (String "teleportToTarget")  = pure teleportToTarget
    parseJSON (String "telekinesis")       = pure telekinesis
    parseJSON (String "toxicCloud")        = pure toxicCloud
    parseJSON (String "triggerTrap")       = pure triggerTrap
    parseJSON (String "unkSp130")          = pure unkSp130
    parseJSON (String "unkSp131")          = pure unkSp131
    parseJSON (String "unkSp132")          = pure unkSp132
    parseJSON (String "unkSp133")          = pure unkSp133
    parseJSON (String "unkSp134")          = pure unkSp134
    parseJSON (String "unkSp135")          = pure unkSp135
    parseJSON (String "teleportToMarker")  = pure teleportToMarker
    parseJSON (String "unkSp137")          = pure unkSp137
    parseJSON (String "unkSp138")          = pure unkSp138
    parseJSON (String "unkSp139")          = pure unkSp139
    parseJSON (String "unkSp140")          = pure unkSp140
    parseJSON (String "unkSp141")          = pure unkSp141
    parseJSON (String "unkSp142")          = pure unkSp142
    parseJSON (String "unkSp143")          = pure unkSp143
    parseJSON _                    = mzero



-- This allows us to "fold mconcat [AllowedSpells]"
instance Monoid AllowedSpells where
    mempty = noFlags
    mappend = (.+.)

refreshConfig confVar = do
    --let newConf = Config { serverStatus = ServerStatus { bannedSpells = [lightning] } }
    Right newConf <- decodeFileEither "config.yaml"
    print "re-read config"
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
    conf <- liftIO $ takeMVar confVar
    liftIO $ refreshConfig confVar

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
