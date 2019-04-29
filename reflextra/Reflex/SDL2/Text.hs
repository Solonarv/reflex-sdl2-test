{-# LANGUAGE FlexibleContexts #-}
module Reflex.SDL2.Text where

import Control.Monad.Reader.Class
import Reflex
import Reflex.SDL2
import qualified SDL.Font as TTF
import Data.Text (Text)


data TextRenderStyle
  = TextSolid TTF.Font TTF.Color
  | TextShaded TTF.Font TTF.Color TTF.Color
  | TextBlended TTF.Font TTF.Color
  deriving (Eq, Show)


renderStyledText :: MonadIO m => TextRenderStyle -> Text -> m Surface
renderStyledText (TextSolid font color) = TTF.solid font color
renderStyledText (TextShaded font fgColor bgColor) = TTF.shaded font fgColor bgColor
renderStyledText (TextBlended font color) = TTF.solid font color
  
renderedTextSurface :: (PerformEvent t m
                       , MonadIO (Performable m)
                       , MonadHold t m
                       , MonadIO (PushM t)
                       )
                    => Dynamic t TextRenderStyle
                    -> Dynamic t Text
                    -> m (Dynamic t Surface)
renderedTextSurface dStyle dText = do
  let dRenderAction = zipDynWith renderStyledText dStyle dText
  dynMapIO id dRenderAction

renderedText :: ( PerformEvent t m
                , MonadIO (Performable m)
                , MonadHold t m
                , MonadIO (PushM t)
                , MonadReader Renderer m
                )
             => Dynamic t TextRenderStyle
             -> Dynamic t Text
             -> m (Dynamic t Texture)
renderedText dStyle dText = do
  dTextSurf <- renderedTextSurface dStyle dText
  r <- ask
  dynMapIO (createTextureFromSurface r) dTextSurf


dynMapIO :: (PerformEvent t m
            , MonadIO (Performable m)
            , MonadHold t m
            , MonadIO (PushM t)
            )
         => (a -> IO b)
         -> Dynamic t a
         -> m (Dynamic t b)
dynMapIO f dA = do
  eUpd <- performEvent (liftIO . f <$> updated dA)
  let mUpd = liftIO . f =<< sample (current dA)
  buildDynamic mUpd eUpd

