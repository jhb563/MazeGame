{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSONInstances where

import Data.Aeson
import qualified Data.Aeson as Ae
import Data.Text (Text, pack)
import Graphics.Gloss.Data.Color
import System.Random (mkStdGen)
import Text.Megaparsec (runParser)

import MazeParser
import Types

instance FromJSON World where
  parseJSON = withObject "World" $ \o -> do
    player <- o .: "player"
    startLoc <- o .: "startLocation"
    endLoc <- o .: "endLocation"
    (boundaryString :: Text) <- o .: "boundaries"
    result <- o .: "result"
    enemies <- o .: "enemies"
    drillLocs <- o .: "drillPowerupLocations"
    stunCells <- o .: "stunCells"
    time <- o .: "time"
    params <- o .: "gameParameters"
    let (rs, cs) = (numRows params, numColumns params)
    let boundaries = case runParser (mazeParser (rs, cs)) "" boundaryString of
          Right result -> result
          _ -> error "Map parse failed!"
    let gen = case randomGeneratorSeed params of
          Just i -> mkStdGen i
          _ -> mkStdGen 1
    return $ World player startLoc endLoc boundaries
      result gen enemies drillLocs stunCells time params

instance ToJSON World where
  toJSON w = object
    [ "player" .= worldPlayer w
    , "startLocation" .= startLocation w
    , "endLocation" .= endLocation w
    , "boundaries" .= dumpMaze (worldBoundaries w)
    , "result" .= worldResult w
    , "enemies" .= worldEnemies w
    , "drillPowerupLocations" .= worldDrillPowerUpLocations w
    , "stunCells" .= stunCells w
    , "time" .= worldTime w
    , "gameParameters" .= worldParameters w
    ]

instance FromJSON Player where
  parseJSON = withObject "Player" $ \o -> do
    location <- o .: "location"
    currentStunDelay <- o .: "currentStunDelay"
    nextStunDelay <- o .: "nextStunDelay"
    drillsRemaining <- o .: "drillsRemaining"
    return $ Player location currentStunDelay nextStunDelay drillsRemaining

instance ToJSON Player where
  toJSON p = object
    [ "location" .= playerLocation p
    , "currentStunDelay" .= playerCurrentStunDelay p
    , "nextStunDelay" .= playerNextStunDelay p
    , "drillsRemaining" .= playerDrillsRemaining p
    ]

instance FromJSON Enemy where
  parseJSON = withObject "Enemy" $ \o -> do
    location <- o .: "location"
    lagTime <- o .: "lagTime"
    nextStunDuration <- o .: "nextStunDuration"
    currentStunTimer <- o .: "currentStunTimer"
    return $ Enemy location lagTime nextStunDuration currentStunTimer

instance ToJSON Enemy where
  toJSON e = object
    [ "location" .= enemyLocation e
    , "lagTime" .= enemyLagTime e
    , "currentStunTimer" .= enemyCurrentStunTimer e
    , "nextStunDuration" .= enemyNextStunDuration e
    ]

instance FromJSON GameResult where
  parseJSON = withText "GameResult" parseText
    where
      parseText "InProgress" = return GameInProgress
      parseText "Won" = return GameWon
      parseText "Lost" = return GameLost
      parseText _ = error "Couldn't parse game result!"

instance ToJSON GameResult where
  toJSON GameInProgress = Ae.String "InProgress"
  toJSON GameWon = Ae.String "Won"
  toJSON GameLost = Ae.String "Lost"

instance FromJSON GameParameters where
  parseJSON = withObject "GameParameters" $ \o -> do
    numRows <- o .: "numRows"
    numCols <- o .: "numColumns"
    numEnemies <- o .: "numEnemies"
    numDrills <- o .: "numDrillPowerups"
    tickRate <- o .: "tickRate"
    playerParams <- o .: "playerParameters"
    enemyParams <- o .: "enemyParameters"
    randomGen <- o .:? "randomSeed"
    return $ GameParameters numRows numCols numEnemies numDrills tickRate
      playerParams enemyParams randomGen

instance ToJSON GameParameters where
  toJSON gp = object
    [ "numRows" .= numRows gp
    , "numColumns" .= numColumns gp
    , "numEnemies" .= numEnemies gp
    , "numDrillPowerups" .= numDrillPowerups gp
    , "tickRate" .= tickRate gp
    , "playerParameters" .= playerGameParameters gp
    , "enemyParameters" .= enemyGameParameters gp
    , "randomSeed" .= randomGeneratorSeed gp
    ]

instance FromJSON PlayerGameParameters where
  parseJSON = withObject "PlayerGameParameters" $ \o -> do
    initialStunTimer <- o .: "initialStunTimer"
    stunIncrease <- o .: "stunTimerIncrease"
    stunMax <- o .: "stunTimerMax"
    radius <- o .: "stunRadius"
    drills <- o .: "initialDrills"
    return $ PlayerGameParameters initialStunTimer stunIncrease stunMax radius drills

instance ToJSON PlayerGameParameters where
  toJSON pp = object
    [ "initialStunTimer" .= initialStunTimer pp
    , "stunTimerIncrease" .= stunTimerIncrease pp
    , "stunTimerMax" .= stunTimerMax pp
    , "stunRadius" .= stunRadius pp
    , "initialDrills" .= initialDrills pp
    ]

instance FromJSON EnemyGameParameters where
  parseJSON = withObject "EnemyGameParameters" $ \o -> do
    initialStun <- o .: "initialStunTime"
    stunDecrease <- o .: "stunTimeDecrease"
    minStun <- o .: "minStunTime"
    randomMove <- o .: "randomMoveChance"
    initialLag <- o .: "initialLagTime"
    minLag <- o .: "minLagTime"
    return $ EnemyGameParameters initialStun stunDecrease minStun randomMove initialLag minLag

instance ToJSON EnemyGameParameters where
  toJSON ep = object
    [ "initialStunTime" .= initialStunTime ep
    , "stunTimeDecrease" .= stunTimeDecrease ep
    , "minStunTime" .= minStunTime ep
    , "randomMoveChance" .= enemyRandomMoveChance ep
    , "initialLagTime" .= initialLagTime ep
    , "minLagTime" .= minLagTime ep
    ]

instance ToJSON RenderParameters where
  toJSON rp = object
    [ "screenDimension" .= screenDimen rp
    , "screenOffsetX" .= screenOffsetX rp
    , "screenOffsetY" .= screenOffsetY rp
    , "textOffset" .= textOffset rp
    , "textScale" .= textScale rp
    , "playerParameters" .= playerRenderParameters rp
    , "enemyParameters" .= enemyRenderParameters rp
    , "cellParameters" .= cellRenderParameters rp
    ]

instance FromJSON RenderParameters where
  parseJSON = withObject "RenderParameters" $ \o -> do
    dimen <- o .: "screenDimension"
    offsetX <- o .: "screenOffsetX"
    offsetY <- o .: "screenOffsetY"
    tOffset <- o .: "textOffset"
    tScale <- o .: "textScale"
    prp <- o .: "playerParameters"
    erp <- o .: "enemyParameters"
    crp <- o .: "cellParameters"
    return $ RenderParameters dimen offsetX offsetY tOffset tScale prp erp crp

instance ToJSON PlayerRenderParameters where
  toJSON prp = object
    [ "size" .= playerIndicatorSize prp
    , "baseColor" .= (ColorWrapper (playerIndicatorColor prp))
    , "stunIndicatorSize" .= playerStunIndicatorSize prp
    , "stunIndicatorColor" .= (ColorWrapper (playerStunIndicatorColor prp))
    , "drillPowerupSize" .= playerDrillPowerupSize prp
    , "drillIndicatorSize" .= playerDrillIndicatorSize prp
    , "drillColor" .= (ColorWrapper (playerDrillColor prp))
    ]

instance FromJSON PlayerRenderParameters where
  parseJSON = withObject "PlayerRenderParameters" $ \o -> do
    size <- o .: "size"
    (ColorWrapper baseColor) <- o .: "baseColor"
    stunSize <- o .: "stunIndicatorSize"
    (ColorWrapper stunColor) <- o .: "stunIndicatorColor"
    drillPowerupSize <- o .: "drillPowerupSize"
    drillIndicatorSize <- o .: "drillIndicatorSize"
    (ColorWrapper drillColor) <- o .: "drillColor"
    return $ PlayerRenderParameters size baseColor stunSize stunColor
      drillPowerupSize drillIndicatorSize drillColor

instance ToJSON EnemyRenderParameters where
  toJSON erp = object
    [ "size" .= enemySize erp
    , "baseColor" .= (ColorWrapper (enemyBaseColor erp))
    , "stunColor" .= (ColorWrapper (enemyStunnedColor erp))
    ]

instance FromJSON EnemyRenderParameters where
  parseJSON = withObject "EnemyRenderParameters" $ \o -> do
    size <- o .: "size"
    (ColorWrapper bc) <- o .: "baseColor"
    (ColorWrapper sc) <- o .: "stunColor"
    return $ EnemyRenderParameters size bc sc

newtype ColorWrapper = ColorWrapper { unColor :: Color }

-- TODO: Add custom color support with arrays.
instance ToJSON ColorWrapper where
  toJSON (ColorWrapper c) = Ae.String colorStr
    where
      colorStr
        | c == blue = "blue"
        | c == red = "red"
        | c == yellow = "yellow"
        | c == green = "green"
        | c == cyan = "cyan"
        | c == orange = "orange"
        | c == magenta = "magenta"
        | c == rose = "rose"
        | c == black = "black"

instance FromJSON ColorWrapper where
  parseJSON = withText "ColorWrapper" parseText
    where
      parseText "blue" = return (ColorWrapper blue)
      parseText "red" = return (ColorWrapper red)
      parseText "yellow" = return (ColorWrapper yellow)
      parseText "green" = return (ColorWrapper green)
      parseText "cyan" = return (ColorWrapper cyan)
      parseText "orange" = return (ColorWrapper orange)
      parseText "magenta" = return (ColorWrapper magenta)
      parseText "rose" = return (ColorWrapper rose)
      parseText "black" = return (ColorWrapper black)
      parseText _ = error "Couldn't parse color!"

instance ToJSON CellRenderParameters where
  toJSON crp = object
    [ "wallColor" .= (ColorWrapper (cellWallColor crp))
    , "stunColor" .= (ColorWrapper (cellStunColor crp))
    , "wallWidth" .= cellWallWidth crp
    ]

instance FromJSON CellRenderParameters where
  parseJSON = withObject "CellRenderParameters" $ \o -> do
    (ColorWrapper wc) <- o .: "wallColor"
    (ColorWrapper sc) <- o .: "stunColor"
    width <- o .: "wallWidth"
    return $ CellRenderParameters wc sc width
