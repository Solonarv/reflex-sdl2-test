{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Fixed (mod')
import Data.Foldable
import Data.Monoid
import Foreign.C.Types
import System.Environment

import CoercibleUtils
import Control.Monad.Reader
import Reflex
import Reflex.SDL2
import Time.Repeatedly

import Reflex.SDL2.Keys
import Reflex.SDL2.Layers

gameSize :: Num a => V2 a
gameSize = V2 800 600

main :: IO ()
main = do
  initializeAll
  let windowCfg = defaultWindow{ windowInitialSize = gameSize }
  window <- createWindow "a moving block - use arrow keys" windowCfg
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  hostLayers r app
  destroyRenderer r
  destroyWindow window
  quit


app :: (ReflexSDL2 t m, MonadLayer t m) => m ()
app = do
  _ <- movableBox arrowKeys 50 100
  shutdownOn =<< delay 0 =<< getQuitEvent

data MoveKeys = MoveKeys
  { mkUp, mkDown, mkLeft, mkRight :: Scancode
  }

arrowKeys :: MoveKeys
arrowKeys = MoveKeys
  { mkUp    = ScancodeUp
  , mkDown  = ScancodeDown
  , mkLeft  = ScancodeLeft
  , mkRight = ScancodeRight
  }

getMoveDirection :: (ReflexSDL2 t m) => MoveKeys -> m (Dynamic t (V2 Sign))
getMoveDirection MoveKeys{..} = do
  dVertical <- axisKeyControl mkUp mkDown
  dHorizontal <- axisKeyControl mkLeft mkRight
  pure $ zipDynWith V2 dHorizontal dVertical

wrapV2 :: Real a => V2 a -> V2 a -> V2 a
wrapV2 = liftA2 (flip mod')

movableBox :: (ReflexSDL2 t m, MonadLayer t m) => MoveKeys -> Double -> V2 Int -> m (Dynamic t (V2 Int))
movableBox keys speed halfSize = do
  dMoveDir <- getMoveDirection keys
  eTick <- getDeltaTickEvent
  ePB <- getPostBuild
  let eUpdate = attachWith update (current dMoveDir) $ leftmost [eTick, 0 <$ ePB]
      update dir dt = \pos -> wrapV2 gameSize (pos + fmap (signedValue speed) dir * fromIntegral dt / 1000)
  dBoxPos <- (fmap . fmap) round <$> foldDyn id 0 eUpdate
  drawLayer $ ffor dBoxPos \pos -> do
    r <- ask
    rendererDrawColor r $= V4 255 0 0 255
    fillRect r (Just (fromIntegral <$> Rectangle (P (pos - halfSize)) (halfSize*2)))
  pure dBoxPos

getPeriodicTick :: (ReflexSDL2 t m) => Double -> m (Event t ())
getPeriodicTick freq = do
  let timer cb = void . liftIO $ asyncRepeatedly (toRational freq) (cb ())
  eBuilt <- getPostBuild
  performEventAsync (timer <$ eBuilt)
  
