{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.SDL2.Layers where

import Data.Foldable

import Control.Monad.Reader
import Reflex
import Reflex.SDL2

type Layer m = Performable m ()
type MonadLayer t m = (MonadReader Renderer (Performable m), MonadReader Renderer m, MonadDynamicWriter t [Layer m] m)

drawLayer :: (ReflexSDL2 t m, MonadLayer t m) => Dynamic t (Layer m) -> m ()
drawLayer = tellDyn . fmap pure

hostLayers :: Renderer -> DynamicWriterT Spider [Layer (ReaderT Renderer ConcreteReflexSDL2)] (ReaderT Renderer ConcreteReflexSDL2) () -> IO ()
hostLayers r guest = host do
  (_, dLayers) <- flip runReaderT r $ runDynamicWriterT guest
  ePB <- getPostBuild
  let eDisplay = leftmost [updated dLayers, [] <$ ePB]
  performEvent_ $ ffor eDisplay \layers -> do
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    runReaderT (sequence_ layers) r
    present r
