{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.SDL2.Keys where

import Control.Monad

import Reflex
import Reflex.SDL2

getIsKeyDown :: ReflexSDL2 t m => Scancode -> m (Dynamic t Bool)
getIsKeyDown key = do
  eKeyAll <- getKeyboardEvent
  let eMyKey = fforMaybe eKeyAll \e -> do
        guard $ key == keysymScancode (keyboardEventKeysym e)
             && not (keyboardEventRepeat e)
        pure (Pressed == keyboardEventKeyMotion e)
  holdDyn False eMyKey

data Sign = Negative | Zero | Positive
  deriving (Eq, Ord, Show)

instance Semigroup Sign where
  Zero <> x = x
  x <> Zero = x
  x <> y
    | x == y    = x
    | otherwise = Zero

instance Monoid Sign where
  mempty = Zero

signedValue :: Num a => a -> Sign -> a
signedValue a Negative = -a
signedValue _ Zero     = 0
signedValue a Positive = a

invertSign :: Sign -> Sign
invertSign Negative = Positive
invertSign Zero     = Zero
invertSign Positive = Positive

axisKeyControl :: ReflexSDL2 t m => Scancode -> Scancode -> m (Dynamic t Sign)
axisKeyControl kNeg kPos = do
  dNeg <- getIsKeyDown kNeg
  dPos <- getIsKeyDown kPos
  let combine True  False = Negative
      combine False True  = Positive
      combine _     _     = Zero
  pure $ zipDynWith combine dNeg dPos
