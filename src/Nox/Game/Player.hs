module Nox.Game.Player where

import Data.Word

type HealthPoints = Word16
type ManaPoints = Word16
type Weight = Word16
type Speed = Word16
type Strength = Word16
type Level = Word8

data Player = Player { hp :: HealthPoints
                     , maxHp :: HealthPoints
                     , mp :: ManaPoints
                     , maxMp :: ManaPoints
                     , weight :: Weight
                     , speed :: Speed
                     , strength :: Strength
                     , level :: Level
                     }

