{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Reflex.SDL2.Text where

import Control.Applicative
import Data.Functor.Identity

import Control.Monad.Reader
import Data.Functor.Misc
import Reflex
import Reflex.SDL2
import qualified SDL.Font as TTF
import Data.Text (Text)

import IOCache (DCache)
import qualified IOCache


data TextRenderStyle
  = TextSolid TTF.Color
  | TextShaded TTF.Color TTF.Color
  | TextBlended TTF.Color
  deriving (Eq, Ord, Show)

data FontSpec = FontSpec 
  { fsPath :: FilePath
  , fsPointSize :: Int
  , fsFaceIndex :: Int
  } deriving (Eq, Ord)

type FontCache m = DCache m (Const2 FontSpec TTF.Font) Identity
type TextCache m = DCache m (Const2 TextRenderRequest Texture) Identity

loadFontSpec :: MonadIO m => Const2 FontSpec TTF.Font a -> m (Identity a)
loadFontSpec (Const2 fs) = Identity <$> liftA3 TTF.loadIndex fsPath fsPointSize fsFaceIndex fs

freeFont :: MonadIO m => Const2 k TTF.Font a -> Identity a -> m ()
freeFont (Const2 _) (Identity fnt) = TTF.free fnt

data TextRenderRequest = TextRenderRequest
 { trrText :: Text
 , trrFont :: FontSpec
 , trrStyle :: TextRenderStyle
 } deriving (Eq, Ord)

renderText :: MonadIO m => Renderer -> DCache m (Const2 FontSpec TTF.Font) Identity -> TextRenderRequest -> m (Identity Texture)
renderText r fntCache (TextRenderRequest txt fntSpec style) = do
  fnt <- IOCache.get' fntCache (Const2 fntSpec)
  fmap Identity . createTextureFromSurface r =<< case style of
    TextSolid fg     -> TTF.solid fnt fg txt
    TextShaded fg bg -> TTF.shaded fnt fg bg txt
    TextBlended fg   -> TTF.blended fnt fg txt

mkRenderText :: MonadIO m => Renderer -> DCache m (Const2 FontSpec TTF.Font) Identity -> Const2 TextRenderRequest Texture a -> m (Identity a)
mkRenderText r fntCache (Const2 req) = renderText r fntCache req

freeTexture :: MonadIO m => Const2 k Texture a -> Identity a -> m ()
freeTexture (Const2 _) (Identity tex) = destroyTexture tex

renderedText :: ( PerformEvent t m
                , MonadIO (Performable m)
                , MonadHold t m
                , MonadIO (PushM t)
                , MonadReader Renderer m
                )
             => DCache IO (Const2 TextRenderRequest Texture) Identity
             -> Dynamic t TextRenderRequest
             -> m (Dynamic t Texture)
renderedText cache dReq = do
  let renderReq req = IOCache.get' cache (Const2 req)
  dynMapIO renderReq dReq

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

