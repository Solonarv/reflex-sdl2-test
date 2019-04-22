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
import Reflex
import Reflex.SDL2
import Time.Repeatedly

gameSize :: Num a => V2 a
gameSize = V2 800 600

main :: IO ()
main = do
  freq <- ffor getArgs \case
    [f] -> read f :: Double
    _   -> 20
  initializeAll
  let windowCfg = defaultWindow{ windowInitialSize = gameSize }
  window <- createWindow "reflex-sdl2-test" windowCfg
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  hostLayers r app
  destroyRenderer r
  destroyWindow window
  quit

type Layer m = Renderer -> Performable m ()
type MonadLayer t m = MonadDynamicWriter t [Layer m] m

drawLayer :: (ReflexSDL2 t m, MonadLayer t m) => Dynamic t (Layer m) -> m ()
drawLayer = tellDyn . fmap pure

hostLayers :: Renderer -> DynamicWriterT Spider [Layer ConcreteReflexSDL2] ConcreteReflexSDL2 () -> IO ()
hostLayers r guest = host do
  (_, dLayers) <- runDynamicWriterT guest
  ePB <- getPostBuild
  let eDisplay = leftmost [updated dLayers, [] <$ ePB]
  performEvent_ $ ffor eDisplay \layers -> do
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    traverse_ ($ r) layers
    present r

app :: (ReflexSDL2 t m, MonadLayer t m) => m ()
app = do
  _ <- movableBox arrowKeys 50 100
  shutdownOn =<< delay 0 =<< getQuitEvent

getIsKeyDown :: ReflexSDL2 t m => Scancode -> m (Dynamic t Bool)
getIsKeyDown key = do
  allKeyEvts <- getKeyboardEvent
  holdDyn False $ fforMaybe allKeyEvts \e -> do
    guard $ key == keysymScancode (keyboardEventKeysym e)
         && not (keyboardEventRepeat e)
    pure (Released == keyboardEventKeyMotion e)

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

getMoveDirection :: (ReflexSDL2 t m, Num a) => MoveKeys -> m (Dynamic t (V2 a))
getMoveDirection MoveKeys{..} = do
  dUp    <- fmap (whenNum (V2   0   1 )) <$> getIsKeyDown mkUp
  dDown  <- fmap (whenNum (V2   0 (-1))) <$> getIsKeyDown mkDown
  dLeft  <- fmap (whenNum (V2   1   0 )) <$> getIsKeyDown mkLeft
  dRight <- fmap (whenNum (V2  (-1) 0 )) <$> getIsKeyDown mkRight
  pure $ ala (Sum . Ap) foldMap [dUp, dDown, dLeft, dRight]

whenNum :: Num a => a -> Bool -> a
whenNum a = \b -> if b then a else 0

wrapV2 :: Real a => V2 a -> V2 a -> V2 a
wrapV2 = liftA2 (flip mod')

movableBox :: (ReflexSDL2 t m, MonadLayer t m) => MoveKeys -> Double -> V2 Int -> m (Dynamic t (V2 Int))
movableBox keys speed halfSize = do
  dMoveDir <- getMoveDirection keys
  eTick <- getDeltaTickEvent
  ePB <- getPostBuild
  let eUpdate = attachWith update (current dMoveDir) $ leftmost [eTick, 0 <$ ePB]
      update dir dt = \pos -> wrapV2 gameSize (pos + dir * V2 speed speed * fromIntegral dt / 1000)
  dBoxPos <- (fmap . fmap) round <$> foldDyn id 0 eUpdate
  drawLayer $ ffor dBoxPos \pos r -> do
    rendererDrawColor r $= V4 255 0 0 255
    fillRect r (Just (fromIntegral <$> Rectangle (P (pos - halfSize)) (halfSize*2)))
  pure dBoxPos

getPeriodicTick :: (ReflexSDL2 t m) => Double -> m (Event t ())
getPeriodicTick freq = do
  let timer cb = void . liftIO $ asyncRepeatedly (toRational freq) (cb ())
  eBuilt <- getPostBuild
  performEventAsync (timer <$ eBuilt)
  