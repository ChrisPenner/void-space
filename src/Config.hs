module Config where

millisecond :: Int
millisecond = 1000

shieldRecoveryTime :: Int
shieldRecoveryTime = 30

shieldRecoveryAmount :: Float
shieldRecoveryAmount = 0.005

enemyDamage :: Float
enemyDamage = 0.3

spawnPercentage :: Float
spawnPercentage = 0.9

corridorWidth :: Int
corridorWidth = 55

tickTime :: Int
tickTime = 300 * millisecond
