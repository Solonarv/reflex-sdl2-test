{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad

import Control.Lens ((^.), over)
import Reflex
import Reflex.SDL2
import System.Random (randomRIO)

import Reflex.SDL2.Layers
import Reflex.SDL2.Keys

gameSize :: Num a => V2 a
gameSize = V2 800 600

main :: IO ()
main = do
  initializeAll
  let windowCfg = defaultWindow{ windowInitialSize = gameSize }
  window <- createWindow "PONG - WS / arrow keys" windowCfg
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  hostLayers r app
  destroyRenderer r
  destroyWindow window
  quit

app :: (ReflexSDL2 t m, MonadLayer t m, MonadIO (PushM t)) => m ()
app = do
  dPaddleL <- verticalPaddle defaultPaddle{paddleXOffset = 100, paddleYRange = 500}
                =<< axisKeyControl ScancodeW  ScancodeS
  dPaddleR <- verticalPaddle defaultPaddle{paddleXOffset = 700, paddleYRange = 500}
                =<< axisKeyControl ScancodeUp ScancodeDown
  (dBall, eScore) <- ball dPaddleL dPaddleR 20
  shutdownOn =<< delay 0 =<< getQuitEvent

data PaddleConfig = PaddleConfig
  { paddleSpeed   :: Double
  , paddleHeight  :: Int
  , paddleXOffset :: Int
  , paddleYRange  :: Int
  } deriving (Eq, Show)

defaultPaddle :: PaddleConfig
defaultPaddle = PaddleConfig
  { paddleSpeed = 300
  , paddleHeight = 100
  , paddleXOffset = 0
  , paddleYRange = 0
  }

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x
  | x < lo = lo
  | x > hi = hi
  | otherwise = x

verticalPaddle :: (ReflexSDL2 t m, MonadLayer t m)
               => PaddleConfig
               -> Dynamic t Sign
               -> m (Dynamic t (Rectangle Int))
verticalPaddle cfg dControl = do
  let startY = maxY / 2
      minY = 0
      maxY = fromIntegral (paddleYRange cfg)
      dSpeed = signedValue (paddleSpeed cfg) <$> dControl
      paddleSize = V2 20 (paddleHeight cfg)
  eTick <- getDeltaTickEvent
  let eFracTick = fmap ((/1000) . fromIntegral) eTick
      eUpdate = attachWith (*) (current dSpeed) eFracTick
  dPaddleY <- fmap round <$> foldDyn (\dy y -> clamp minY maxY $ dy + y) startY eUpdate
  drawLayer $ ffor dPaddleY \y r -> do
    rendererDrawColor r $= V4 255 255 0 255
    fillRect r (Just (fromIntegral <$>
                        Rectangle (P (V2 (paddleXOffset cfg - 10) y))
                                          paddleSize))
  pure $ ffor dPaddleY \y -> Rectangle (P (V2 (paddleXOffset cfg - 10) y)) paddleSize

data Side = LeftPlayer | RightPlayer

data Ball a = Ball
  { ballPos :: V2 a
  , ballVel :: V2 a
  } deriving (Show, Functor)

ball :: forall t m. (ReflexSDL2 t m, MonadLayer t m, MonadIO (PushM t))
     => Dynamic t (Rectangle Int)
     -> Dynamic t (Rectangle Int)
     -> Int
     -> m (Dynamic t (Ball Int), Event t Side)
ball dLeftPaddle dRightPaddle rad = do
  let radius = fromIntegral rad
      colliding leftPaddle rightPaddle pos
        = rectsIntersect (fromIntegral <$> leftPaddle) (ballRect pos)
          || rectsIntersect (fromIntegral <$> rightPaddle) (ballRect pos)
      dPaddleCollider = zipDynWith colliding dLeftPaddle dRightPaddle

      startPos = gameSize / 2

      ballSizeHalf = V2 radius radius
      ballSize = 2 * ballSizeHalf

      ballRect pos = Rectangle (P (pos - ballSizeHalf)) ballSize
      
      randomVel :: MonadIO m' => m' (V2 Double)
      randomVel = liftIO do
        u <- angle <$> randomRIO (0, 2*pi)
        l <- randomRIO (70, 130)
        pure (l *^ u)
  
  eFracTick  <- fmap ((/1000) . fromIntegral) <$> getDeltaTickEvent 
  ePostBuild <- getPostBuild

  let dIsCollidingPaddle dBallPos = zipDynWith ($) dPaddleCollider dBallPos
      ePaddleCollision   dBallPos = fforMaybe (updated (dIsCollidingPaddle dBallPos)) guard
      eVBoundsCollision  dBallPos = fforMaybe (updated (dBallPos))
                                      \pos -> let closeTop = pos ^._y < radius
                                                  closeBot = pos ^._y > gameSize ^._y - radius
                                              in guard (closeTop || closeBot)
      eHBoundsCollision  dBallPos = fforMaybe (updated dBallPos)
                                      \pos -> let closeL = pos ^._x < 0
                                                  closeR = pos ^._x > gameSize ^._x
                                              in (RightPlayer <$ guard closeL)
                                                <|> (LeftPlayer <$ guard closeR)
      
  rec dBallPos :: Dynamic t (V2 Double) <- foldDyn ($) startPos =<< delay 0 ePosUpdate
      
      let ePosUpdate :: Event t (V2 Double -> V2 Double)
          ePosUpdate = leftmost [eStart, eReset, eIntegral]

          eIntegral = (+) <$> attachWith (^*) (current dBallVel) eFracTick
          eStart    = const startPos <$ ePostBuild
          eReset    = const startPos <$ eHBoundsCollision dBallPos
      
      dBallVel :: Dynamic t (V2 Double) <- foldDyn ($) 0 =<< delay 0 eVelUpdate
      
      eVelUpdate :: Event t (V2 Double -> V2 Double) <- do
        let eBounce = mergeWith (.)
              [ over _x negate <$ ePaddleCollision dBallPos
              , over _y negate <$ eVBoundsCollision dBallPos
              ]
        eStart <- performEvent (randomVel <$ ePostBuild)
        eReset <- performEvent (randomVel <$ eHBoundsCollision dBallPos)
        pure $ leftmost [const <$> eStart, const <$> eReset, eBounce]
  drawLayer $ ffor dBallPos \pos r -> do
    rendererDrawColor r $= V4 255 0 0 255
    fillRect r (Just (round <$> ballRect pos))
  pure (fmap round <$> zipDynWith Ball dBallPos dBallVel, eHBoundsCollision dBallPos)

rectsIntersect :: (Num a, Ord a) => Rectangle a -> Rectangle a -> Bool
rectsIntersect (Rectangle (P p) (V2 x y)) r2 = any (`inRect` r2)
  [ p
  , p + V2 x 0
  , p + V2 x y
  , p + V2 0 y
  ]

inRect :: (Num a, Ord a) => V2 a -> Rectangle a -> Bool
inRect q (Rectangle (P p) (V2 dx dy)) = inX && inY
  where
    V2 x y = q - p
    inX = 0 <= x && x <= dx
    inY = 0 <= y && y <= dy
