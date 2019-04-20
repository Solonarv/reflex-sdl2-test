{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative
import Control.Monad (guard)
import Data.Fixed (mod')
import Data.Monoid
import Foreign.C.Types

import CoercibleUtils
import Reflex
import Reflex.SDL2

gameSize :: Num a => V2 a
gameSize = V2 800 600

main :: IO ()
main = do
  initializeAll
  let windowCfg = defaultWindow{ windowInitialSize = gameSize }
  window <- createWindow "reflex-sdl2-test" windowCfg
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  host (app r)
  destroyRenderer r
  destroyWindow window
  quit


app :: ReflexSDL2 t m => Renderer -> m ()
app r = do
  dPlayerPos <- getPlayerPos
  performEvent_ $ ffor (updated dPlayerPos) \pos -> do
    -- liftIO $ putStrLn "state changed, rendering"
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    drawPlayerAt r pos
    present r
  shutdownOn =<< delay 0 =<< getQuitEvent

drawPlayerAt :: MonadIO m => Renderer -> V2 Double -> m ()
drawPlayerAt r (fmap floor -> pos) = do
  rendererDrawColor r $= V4 255 0 0 255
  fillRect r (Just (Rectangle (P (pos - 60)) (120)))

getIsKeyDown :: ReflexSDL2 t m => Scancode -> m (Dynamic t Bool)
getIsKeyDown key = do
  allKeyEvts <- getKeyboardEvent
  holdDyn False $ fforMaybe allKeyEvts \e -> do
    guard $ key == keysymScancode (keyboardEventKeysym e)
         && not (keyboardEventRepeat e)
    pure (Released == keyboardEventKeyMotion e)

getMoveDirection :: (ReflexSDL2 t m, Num a) => m (Dynamic t (V2 a))
getMoveDirection = do
  dUp    <- fmap (whenNum (V2   0   1 )) <$> getIsKeyDown ScancodeUp
  dDown  <- fmap (whenNum (V2   0 (-1))) <$> getIsKeyDown ScancodeDown
  dLeft  <- fmap (whenNum (V2   1   0 )) <$> getIsKeyDown ScancodeLeft
  dRight <- fmap (whenNum (V2  (-1) 0 )) <$> getIsKeyDown ScancodeRight
  pure $ ala (Sum . Ap) foldMap [dUp, dDown, dLeft, dRight]

whenNum :: Num a => a -> Bool -> a
whenNum a = \b -> if b then a else 0

getPlayerPos :: (ReflexSDL2 t m) => m (Dynamic t (V2 Double))
getPlayerPos = do
  eTick <- getPeriodicTick 60
  dMoveDir <- getMoveDirection
  let posChange = current dMoveDir <@ eTick
  fmap (wrapV2 gameSize) <$> foldDyn (+) 0 ((*10) <$> posChange)

wrapV2 :: Real a => V2 a -> V2 a -> V2 a
wrapV2 = liftA2 (flip mod')

getPeriodicTick :: (ReflexSDL2 t m) => Rational -> m (Event t TickInfo)
getPeriodicTick freq = tickLossyFromPostBuildTime (fromRational (recip freq))