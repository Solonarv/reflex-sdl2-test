{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Functor

import Control.Lens ((^.), over)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex
import Reflex.SDL2
import qualified SDL.Font as TTF
import System.Random (randomRIO, randomIO)

import Reflex.SDL2.Layers
import Reflex.SDL2.Keys
import Reflex.SDL2.Text

gameSize :: Num a => V2 a
gameSize = V2 800 600

main :: IO ()
main = do
  initializeAll
  TTF.initialize
  let windowCfg = defaultWindow{ windowInitialSize = gameSize }
  window <- createWindow "PONG - WS / arrow keys" windowCfg
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  hostLayers r app
  destroyRenderer r
  destroyWindow window
  TTF.quit
  quit

app :: (ReflexSDL2 t m, MonadLayer t m, MonadSample t (Performable m), MonadIO (PushM t)) => m ()
app = do
  dPaddleL <- verticalPaddle defaultPaddle{paddleXOffset = 100, paddleYRange = 500}
                =<< axisKeyControl ScancodeW  ScancodeS
  dPaddleR <- verticalPaddle defaultPaddle{paddleXOffset = 700, paddleYRange = 500}
                =<< axisKeyControl ScancodeUp ScancodeDown
  (dBall, ePoint) <- ball dPaddleL dPaddleR 20
  dScore <- trackScores ePoint
  renderScore dScore
  eQuit <- getQuitEvent
  performEvent_ $ eQuit $> do
    Scores scoreL scoreR <- sample (current dScore)
    liftIO . putStrLn $ concat
      [ "Final score: Left "
      , show scoreL
      , " - "
      , show scoreR
      , " Right"
      ]
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
  drawLayer $ ffor dPaddleY \y -> do
    r <- ask
    rendererDrawColor r $= V4 255 255 0 255
    fillRect r (Just (fromIntegral <$>
                        Rectangle (P (V2 (paddleXOffset cfg - 10) y))
                                          paddleSize))
  pure $ ffor dPaddleY \y -> Rectangle (P (V2 (paddleXOffset cfg - 10) y)) paddleSize

data Player = LeftPlayer | RightPlayer

data Scores = Scores !Int !Int

trackScores :: ReflexSDL2 t m => Event t Player -> m (Dynamic t Scores)
trackScores = foldDyn addPoint (Scores 0 0)
  where
    addPoint LeftPlayer (Scores l r) = Scores (l+1) r
    addPoint RightPlayer (Scores l r) = Scores l (r+1)

data Edge = LeftEdge | RightEdge | TopEdge | BottomEdge

edgePlayer :: Edge -> Maybe Player
edgePlayer LeftEdge  = Just LeftPlayer
edgePlayer RightEdge = Just RightPlayer
edgePlayer _         = Nothing

ball :: forall t m. (ReflexSDL2 t m, MonadLayer t m)
     => Dynamic t (Rectangle Int)
     -> Dynamic t (Rectangle Int)
     -> Int
     -> m (Dynamic t (V2 Int), Event t Player)
ball dLeftPaddle dRightPaddle rad = do
  let radius = fromIntegral rad
      colliding leftPaddle rightPaddle pos
        = LeftEdge <$ guard (rectsIntersect (fromIntegral <$> leftPaddle) (ballRect pos))
          <|> RightEdge <$ guard (rectsIntersect (fromIntegral <$> rightPaddle) (ballRect pos))
      dPaddleCollider = zipDynWith colliding dLeftPaddle dRightPaddle

      startPos = gameSize / 2

      ballSizeHalf = V2 radius radius
      ballSize = 2 * ballSizeHalf

      ballRect pos = Rectangle (P (pos - ballSizeHalf)) ballSize
      
      reflect LeftEdge   = over _x abs
      reflect RightEdge  = over _x (negate . abs)
      reflect TopEdge    = over _y abs
      reflect BottomEdge = over _y (negate . abs)

      randomVel :: MonadIO m' => m' (V2 Double)
      randomVel = liftIO do
        V2 ux uy <- angle <$> randomRIO (-pi/4, pi/4)
        l <- randomRIO (70, 130)
        dir <- randomIO
        pure if dir then l *^ V2 ux uy else l *^ V2 (-ux) uy
  
  eFracTick  <- fmap ((/1000) . fromIntegral) <$> getDeltaTickEvent 
  ePostBuild <- getPostBuild

  let dIsCollidingPaddle dBallPos = zipDynWith ($) dPaddleCollider dBallPos
      ePaddleCollision   dBallPos = fforMaybe (updated (dIsCollidingPaddle dBallPos)) id
      eVBoundsCollision  dBallPos = fforMaybe (updated (dBallPos))
                                      \pos -> let closeTop = pos ^._y < radius
                                                  closeBot = pos ^._y > gameSize ^._y - radius
                                              in TopEdge <$ guard closeTop
                                                <|> BottomEdge <$ guard closeBot
      eHBoundsCollision  dBallPos = fforMaybe (updated dBallPos)
                                      \pos -> let closeL = pos ^._x < 0
                                                  closeR = pos ^._x > gameSize ^._x
                                              in RightPlayer <$ guard closeL
                                                <|> LeftPlayer <$ guard closeR
      
  rec dBallPos :: Dynamic t (V2 Double) <- foldDyn ($) startPos =<< delay 0 ePosUpdate
      
      let ePosUpdate :: Event t (V2 Double -> V2 Double)
          ePosUpdate = leftmost [eStart, eReset, eIntegral]

          eIntegral = (+) <$> attachWith (^*) (current dBallVel) eFracTick
          eStart    = const startPos <$ ePostBuild
          eReset    = const startPos <$ eHBoundsCollision dBallPos
      
      dBallVel :: Dynamic t (V2 Double) <- foldDyn ($) 0 =<< delay 0 eVelUpdate
      
      eVelUpdate :: Event t (V2 Double -> V2 Double) <- do
        let eBounce = mergeWith (.)
              [ reflect <$> ePaddleCollision dBallPos
              , reflect <$> eVBoundsCollision dBallPos
              ]
        eStart <- performEvent (randomVel <$ ePostBuild)
        eReset <- performEvent (randomVel <$ eHBoundsCollision dBallPos)
        pure $ leftmost [const <$> eStart, const <$> eReset, eBounce]
  drawLayer $ ffor dBallPos \pos -> do
    r <- ask
    rendererDrawColor r $= V4 255 0 0 255
    fillRect r (Just (round <$> ballRect pos))
  pure ( fmap round <$> dBallPos
       , eHBoundsCollision dBallPos)

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

renderScore :: (ReflexSDL2 t m, MonadLayer t m, MonadIO (PushM t)) => Dynamic t Scores -> m ()
renderScore dScore = do
  dStyle <- flip buildDynamic never $ do
    font <- TTF.load "OpenSans-Regular.ttf" 32
    pure (TextBlended font (V4 192 192 255 255))
  dText <- renderedText dStyle $ ffor dScore \(Scores l r) -> mconcat [tshow l, " | ", tshow r]
  drawLayer $ ffor dText \tex -> do
    info <- queryTexture tex
    r <- ask
    let dest = Rectangle (P destV) (V2 w h)
        w = textureWidth info
        h = textureHeight info
        destV = V2 destX 0
        destX = (gameSize^._x - w) `div` 2
    copy r tex Nothing (Just dest)    

tshow :: Show a => a -> Text
tshow = Text.pack . show
