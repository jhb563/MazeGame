module OptionsParser where

import Data.Aeson (decodeFileStrict')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Graphics.Gloss
import Options.Applicative

import JSONInstances ()
import Types

data RenderParamInfo = RenderParamInfo
  -- Screen/Text Parameters
  (Maybe Int)
  (Maybe Int)
  (Maybe Int)
  (Maybe Float)
  (Maybe Float)
  (Maybe Float)
  (Maybe Float)
  -- Player Parameters
  (Maybe Float)
  (Maybe Color)
  (Maybe Float)
  (Maybe Color)
  (Maybe Float)
  (Maybe Float)
  (Maybe Color)
  -- Enemy Parameters
  (Maybe Float)
  (Maybe Color)
  (Maybe Color)
  -- Cell Parameters
  (Maybe Color)
  (Maybe Color)
  (Maybe Float)

parseOptions :: IO (Maybe FilePath, Bool, RenderParameters)
parseOptions = do
  (mazeFile, renderFile, renderInfo, usePlayerAI) <- execParser $ info parser commandInfo
  case renderFile of
    Nothing -> return (mazeFile, usePlayerAI, mergeOptions defaultRenderParameters renderInfo)
    Just fp -> do
      parseResult <- decodeFileStrict' fp
      case parseResult of
        Nothing -> return (mazeFile, usePlayerAI, mergeOptions defaultRenderParameters renderInfo)
        Just fileRenderParams -> return (mazeFile, usePlayerAI, mergeOptions fileRenderParams renderInfo)

parser :: Parser (Maybe FilePath, Maybe FilePath, RenderParamInfo, Bool)
parser = (,,,) <$>
  mazeFileParser <*>
  renderFileParser <*>
  parseRenderInfo <*>
  usePlayerAIInfo

mazeFileParser :: Parser (Maybe FilePath)
mazeFileParser = maybeParser str (long "load-file" <> short 'f' <> help "A file to use to load the world state")

renderFileParser :: Parser (Maybe FilePath)
renderFileParser = maybeParser str (long "render-param-file" <> short 'r' <> help "A file to use to load render parameters")

usePlayerAIInfo :: Parser Bool
usePlayerAIInfo = switch (long "use-player-ai" <> help "Whether or not to use the AI version of the player instead of input")

parseRenderInfo :: Parser RenderParamInfo
parseRenderInfo = RenderParamInfo <$>
  maybeIntParser (long "screen-dimen" <> help "The screen width/height") <*>
  maybeIntParser (long "screen-offset-x" <> help "The screen width/height") <*>
  maybeIntParser (long "screen-offset-y" <> help "The screen width/height") <*>
  maybeFloatParser (long "text-offset-x" <> help "The text offset x dimension") <*>
  maybeFloatParser (long "text-offset-y" <> help "The text offset y dimension") <*>
  maybeFloatParser (long "text-scale-x" <> help "The text scale x dimension") <*>
  maybeFloatParser (long "text-scale-y" <> help "The text scale y dimension") <*>
  maybeFloatParser (long "player-size" <> help "The player indicator size") <*>
  maybeColorParser (long "player-color" <> help "The player indicator color") <*>
  maybeFloatParser (long "stun-indicator-size" <> help "The player's stun indicator size") <*>
  maybeColorParser (long "stun-indicator-color" <> help "The player's stun indicator color") <*>
  maybeFloatParser (long "drill-size" <> help "The size of drill powerups") <*>
  maybeFloatParser (long "drill-indicator-size" <> help "The player's drill indicator size") <*>
  maybeColorParser (long "" <> help "The drill powerup and indicator color") <*>
  maybeFloatParser (long "enemy-size" <> help "The enemy indicator size") <*>
  maybeColorParser (long "enemy-base-color" <> help "The enemy's normal color") <*>
  maybeColorParser (long "enemy-stun-color" <> help "The enemy's stunned color") <*>
  maybeColorParser (long "cell-wall-color" <> help "The color of cell walls") <*>
  maybeColorParser (long "cell-stun-color" <> help "The color of stunned cells") <*>
  maybeFloatParser (long "cell-wall-width" <> help "The width of cell walls")

mergeOptions :: RenderParameters -> RenderParamInfo -> RenderParameters
mergeOptions
  rp
  (RenderParamInfo sd_ sox_ soy_ tox_ toy_ tsx_ tsy_ pis_ pic_ psis_ psic_ dps_ dis_ dc_ es_ ebc_ esc_ cwc_ csc_ cww_)
  = RenderParameters
    (fromMaybe (screenDimen rp) sd_)
    (fromMaybe (screenOffsetX rp) sox_)
    (fromMaybe (screenOffsetY rp) soy_)
    (fromMaybe (fst . textOffset $ rp) tox_, fromMaybe (snd . textOffset $ rp) toy_)
    (fromMaybe (fst . textScale $ rp) tsx_, fromMaybe (snd . textScale $ rp) tsy_)
    (PlayerRenderParameters
      (fromMaybe (playerIndicatorSize prp) pis_)
      (fromMaybe (playerIndicatorColor prp) pic_)
      (fromMaybe (playerStunIndicatorSize prp) psis_)
      (fromMaybe (playerStunIndicatorColor prp) psic_)
      (fromMaybe (playerDrillPowerupSize prp) dps_)
      (fromMaybe (playerDrillIndicatorSize prp) dis_)
      (fromMaybe (playerDrillColor prp) dc_)
    )
    (EnemyRenderParameters
      (fromMaybe (enemySize erp) es_)
      (fromMaybe (enemyBaseColor erp) ebc_)
      (fromMaybe (enemyStunnedColor erp) esc_)
    )
    (CellRenderParameters
      (fromMaybe (cellWallColor crp) cwc_)
      (fromMaybe (cellStunColor crp) csc_)
      (fromMaybe (cellWallWidth crp) cww_)
    )
  where
    prp = playerRenderParameters rp
    erp = enemyRenderParameters rp
    crp = cellRenderParameters rp

-- Helpers

maybeIntParser :: Mod OptionFields (Maybe Int) -> Parser (Maybe Int)
maybeIntParser = maybeParser auto

maybeFloatParser :: Mod OptionFields (Maybe Float) -> Parser (Maybe Float)
maybeFloatParser = maybeParser auto

maybeColorParser :: Mod OptionFields (Maybe Color) -> Parser (Maybe Color)
maybeColorParser = maybeParser (maybeReader colorReader)
  where
    colorReader "blue" = Just blue
    colorReader "red" = Just red
    colorReader "yellow" = Just yellow
    colorReader "green" = Just green
    colorReader "cyan" = Just cyan
    colorReader "orange" = Just orange
    colorReader "magenta" = Just magenta
    colorReader "rose" = Just rose
    colorReader "black" = Just black
    colorReader _ = Nothing

maybeParser :: ReadM a -> Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeParser reader opts = option (Just <$> reader) (opts <> value Nothing)

commandInfo :: InfoMod (Maybe FilePath, Maybe FilePath, RenderParamInfo, Bool)
commandInfo = fullDesc <> progDesc "Haskell Maze Game"
