{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.SDL2.Layers where

import Data.Foldable

import Reflex
import Reflex.SDL2

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
